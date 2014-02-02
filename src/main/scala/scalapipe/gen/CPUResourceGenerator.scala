package scalapipe.gen

import scalapipe._

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import java.io.File

private[scalapipe] class CPUResourceGenerator(
        val sp: ScalaPipe,
        val host: String
    ) extends ResourceGenerator {

    private val edgeGenerators = new HashMap[EdgeGenerator, HashSet[Stream]]
    private val emittedKernelTypes = new HashSet[KernelType]
    private val threadIds = new HashMap[KernelInstance, Int]

    private lazy val openCLEdgeGenerator = new OpenCLEdgeGenerator(sp)
    private lazy val smartFusionEdgeGenerator = new SmartFusionEdgeGenerator(sp)
    private lazy val simulationEdgeGenerator = new SimulationEdgeGenerator(sp)
    private lazy val sockEdgeGenerator = new SockEdgeGenerator(sp, host)
    private lazy val cEdgeGenerator = new CEdgeGenerator

    private def getHDLEdgeGenerator: EdgeGenerator = {
        val fpga = sp.parameters.get[String]('fpga)
        fpga match {
            case "SmartFusion"    => smartFusionEdgeGenerator
            case "Simulation"     => simulationEdgeGenerator
            case _ =>
                Error.raise(s"unknown FPGA type: $fpga")
                simulationEdgeGenerator
        }
    }

    private def addEdgeGenerator(stream: Stream) {

        val dest = stream.destKernel.device
        val src = stream.sourceKernel.device

        val generator: EdgeGenerator = stream.edge match {
            case c2f: CPU2FPGA                          => getHDLEdgeGenerator
            case f2c: FPGA2CPU                          => getHDLEdgeGenerator
            case c2g: CPU2GPU                           => openCLEdgeGenerator
            case g2c: GPU2CPU                           => openCLEdgeGenerator
            case c2c: CPU2CPU if dest.host != src.host  => sockEdgeGenerator
            case _                                      => cEdgeGenerator
        }

        if (!edgeGenerators.contains(generator)) {
            edgeGenerators += (generator -> new HashSet[Stream])
        }
        edgeGenerators(generator) += stream

    }

    private def shouldEmit(device: Device): Boolean = {
        device.platform == Platforms.C && device.host == host
    }

    private def emitKernelHeader(kernel: KernelInstance) {
        val kernelType = kernel.kernelType
        if (!emittedKernelTypes.contains(kernelType)) {
            emittedKernelTypes += kernelType
            val name = kernelType.name
            write("#include \"" + name + "/" + name + ".h\"")
        }
    }

    private def emitKernelStruct(kernel: KernelInstance) {

        val inputCount = kernel.getInputs.size
        val outputCount = kernel.getOutputs.size

        write(s"static struct {")
        enter
        write(s"SPC clock;")
        write(s"volatile int active_inputs;")
        write(s"SPKernelData data;")
        write(s"struct sp_${kernel.kernelType.name}_data priv;")
        leave
        write(s"} ${kernel.label};")

    }

    private def emitKernelInit(kernel: KernelInstance) {

        val name = kernel.name
        val kernelType = kernel.kernelType
        val instance = kernel.label
        val inPortCount = kernel.getInputs.size
        val outPortCount = kernel.getOutputs.size

        // AP_block_data
        write(s"$instance.active_inputs = $inPortCount;")
        write(s"$instance.data.in_port_count = $inPortCount;")
        write(s"$instance.data.out_port_count = $outPortCount;")
        write(s"$instance.data.get_free = ${instance}_get_free;")
        write(s"$instance.data.allocate = ${instance}_allocate;")
        write(s"$instance.data.send = ${instance}_send;")
        write(s"$instance.data.get_available = ${instance}_get_available;")
        write(s"$instance.data.read_value = ${instance}_read_value;")
        write(s"$instance.data.release = ${instance}_release;")
        write(s"$instance.data.instance = ${kernel.index};")

        // Default config options.
        for (c <- kernel.kernelType.configs) {
            val name = c.name
            val t = c.valueType.baseType
            val custom = kernel.getConfig(name)
            val value = if (custom != null) custom else c.value
            if (value != null) {
                val lit = kernel.kernelType.getLiteral(value)
                write(s"$instance.priv.$name = ($t)$lit;")
            }
        }

        // Call the init function.
        write(s"spc_init(&${instance}.clock);")
        write(s"sp_${name}_init(&$instance.priv);")

    }

    private def emitKernelGetFree(kernel: KernelInstance) {

        val instance = kernel.label

        write(s"static int ${instance}_get_free(int out_port)")
        write(s"{")
        enter
        write(s"switch(out_port) {")
        for (stream <- kernel.getOutputs) {
            val index = kernel.outputIndex(stream.sourcePort)
            write(s"case $index:")
            enter
            write(s"return ${stream.label}_get_free();")
            leave
        }
        write(s"}")
        write(s"return 0;")
        leave
        write(s"}")

    }

    private def emitKernelAllocate(kernel: KernelInstance) {

        val instance = kernel.label
        val rid = threadIds(kernel)

        write(s"static void *${instance}_allocate(int out_port)")
        write(s"{")
        enter
        write(s"spc_stop(&$instance.clock);")
        write(s"void *ptr = NULL;")
        if (!kernel.getOutputs.filter(_.useFull).isEmpty) {
            write(s"bool first = true;")
        }
        write(s"for(;;) {")
        enter

        write(s"switch(out_port) {")
        for (stream <- kernel.getOutputs) {
            val index = kernel.outputIndex(stream.sourcePort)
            write(s"case $index:")
            enter
            write(s"ptr = ${stream.label}_allocate();")
            if (stream.useFull) {
                write(s"if(first && ptr == NULL) {")
                enter
                write(s"first = false;")
                write(s"tta.LogEvent($rid, ${stream.index}, XTTA_TYPE_FULL);")
                leave
                write(s"}")
            }
            write(s"break;")
            leave
        }
        write(s"}")
        write(s"if(SPLIKELY(ptr != NULL)) {")
        enter
        write(s"spc_start(&$instance.clock);")
        write(s"return ptr;")
        leave
        write(s"}")
        write(s"sched_yield();");
        leave
        write(s"}")
        leave
        write(s"}")

    }

    private def emitKernelSend(kernel: KernelInstance) {

        val instance = kernel.label
        val minDepth = kernel.getOutputs.foldLeft(Int.MaxValue) {
            (a, s) => scala.math.min(a, s.depth)
        }
        val rid = threadIds(kernel)

        write(s"static void ${instance}_send(int out_port)")
        write(s"{")
        enter
        val outputCount = kernel.getOutputs.size
        if (outputCount > 0) {
            write(s"spc_stop(&$instance.clock);")
            write(s"switch(out_port) {")
            for (stream <- kernel.getOutputs) {
                val index = kernel.outputIndex(stream.sourcePort)
                write(s"case $index:")
                enter
                if (stream.usePush) {
                    write(s"for(int i = 0; i < count; i++) {")
                    enter
                    write(s"tta.LogEvent($rid, ${stream.index}, " +
                          s"XTTA_TYPE_PUSH, " +
                          s"${stream.label}_get_free() == 0);")
                    leave
                    write(s"}")
                }
                write(s"${stream.label}_send();")
                write(s"break;")
                leave
            }
            write(s"}")
            write(s"spc_start(&$instance.clock);")
        }
        leave
        write(s"}")

    }

    private def emitKernelAvailable(kernel: KernelInstance) {

        val instance = kernel.label

        write(s"static int ${instance}_get_available(int in_port)")
        write(s"{")
        enter
        write(s"int result = 0;")
        write(s"spc_stop(&$instance.clock);")
        if (!kernel.getInputs.isEmpty) {
            write(s"switch(in_port) {")
            for (stream <- kernel.getInputs) {
                val index = kernel.inputIndex(stream.destPort)
                write(s"case $index:")
                enter
                write(s"result = ${stream.label}_get_available();")
                write(s"break;")
                leave
            }
            write(s"}")
        }
        write(s"spc_start(&$instance.clock);")
        write(s"return result;")
        leave
        write(s"}")

    }

    private def emitKernelRead(kernel: KernelInstance) {

        val instance = kernel.label

        write(s"static void *${instance}_read_value(int in_port)")
        write(s"{")
        enter
        write(s"void *ptr = NULL;")
        write(s"spc_stop(&$instance.clock);")
        write(s"for(;;) {")
        enter
        if (!kernel.getInputs.isEmpty) {
            write(s"switch(in_port) {")
            for (stream <- kernel.getInputs) {
                val index = kernel.inputIndex(stream.destPort)
                write(s"case $index:")
                enter
                write(s"ptr = ${stream.label}_read_value();")
                write(s"break;")
                leave
            }
            write(s"}")
        }
        write(s"if(SPLIKELY(ptr != NULL)) {")
        enter
        write(s"spc_start(&$instance.clock);")
        write(s"return ptr;")
        leave
        write(s"}")
        write(s"sched_yield();")
        leave
        write(s"}")
        leave
        write(s"}")

    }

    private def emitKernelRelease(kernel: KernelInstance) {

        val name = kernel.kernelType.name
        val instance = kernel.label

        write(s"static void ${instance}_release(int in_port)")
        write(s"{")
        enter
        write(s"spc_stop(&$instance.clock);")
        write(s"switch(in_port) {")
        for (stream <- kernel.getInputs) {
            val index = kernel.inputIndex(stream.destPort)
            write(s"case $index:")
            enter
            if (stream.usePop) {
                val rid = threadIds(kernel)
                write(s"for(int i = 0; i < count; i++) {")
                enter
                write(s"tta.LogEvent($rid, ${stream.index}, " +
                      s"XTTA_TYPE_POP, ${stream.label}_get_free() == 0);")
                leave
                write(s"}")
            }
            write(s"${stream.label}_release();")
            write(s"break;")
            leave
        }
        write(s"}")
        write(s"spc_start(&$instance.clock);")
        leave
        write(s"}")

    }

    private def emitKernelDestroy(kernel: KernelInstance) {
        val name = kernel.kernelType.name
        val instance = kernel.label
        write(s"sp_${name}_destroy(&$instance.priv);")
    }

    private def emitThread(kernel: KernelInstance) {

        val id = threadIds(kernel)
        val name = kernel.kernelType.name
        val instance = kernel.label

        write(s"static void *run_thread$id(void *arg)")
        write(s"{")
        enter
        write(s"spc_start(&$instance.clock);")
        write(s"sp_${name}_run(&$instance.priv);")
        write(s"spc_stop(&$instance.clock);")
        kernel.getOutputs.filter {
            _.destKernel.device == kernel.device
        }.map(_.destKernel.label).foreach { dest =>
            write(s"$dest.active_inputs -= 1;")
        }
        write(s"return NULL;")
        leave
        write(s"}")

    }

    private def writeShutdown(instances: Traversable[KernelInstance],
                              edgeStats: ListBuffer[Generator]) {

        def writeKernelStats(k: KernelInstance) {
            write(s"ticks = ${k.label}.clock.total_ticks;");
            write(s"pushes = ${k.label}.clock.count;")
            write(s"us = (ticks * total_us) / total_ticks;")
            write(s"""fprintf(stderr, \"     ${k.kernelType.name}(""" +
                  s"""${k.label}): %llu ticks, %llu pushes, %llu us\\n\", """ +
                  s"""ticks, pushes, us);""")
            if (k.kernelType.parameters.get('profile)) {
                write(s"""fprintf(stderr, \"        HDL Clocks: %lu\\n\", """ +
                      s"""${k.label}.priv.sp_clocks);""")
            }
            write(s"if (show_extra_stats) {")
            enter
            k.getInputs.foreach { i =>
                val index = k.inputIndex(i)
                write(s"q_size = q_${i.label}->depth;")
                write(s"q_usage = spq_get_used(q_${i.label});")
                write(s"""fprintf(stderr, \"          Input $index: """ +
                      s"""%llu / %llu\\n\", q_usage, q_size);""")
            }
            leave
            write(s"}")
        }

        write("static void showStats()")
        write("{")
        enter
        write("struct timeval stop_time;")
        write("unsigned long long q_usage;")
        write("unsigned long long q_size;")
        write("unsigned long long ticks;")
        write("unsigned long long pushes;")
        write("unsigned long long us;")
        write("unsigned long long total_ticks;")
        write("unsigned long long total_us;")
        write("unsigned long long stop_ticks = sp_get_ticks();")
        write("gettimeofday(&stop_time, NULL);")
        write("total_ticks = stop_ticks - start_ticks;")
        write("total_us = (stop_time.tv_sec - start_time.tv_sec) * 1000000")
        write("            + (stop_time.tv_usec - start_time.tv_usec);")
        write("""fprintf(stderr, "Statistics:\n");""")
        write("fprintf(stderr, \"Total CPU ticks: %llu\\n\", total_ticks);")
        write("fprintf(stderr, \"Total time:        %llu us\\n\", total_us);")
        instances.foreach(i => writeKernelStats(i))
        write(edgeStats)
        leave
        write("}")

        write("static void shutdown(int s)")
        write("{")
        enter
        write("show_extra_stats = true;")
        write("""fprintf(stderr, "Shutting down...\n");""")
        write("exit(0);")
        leave
        write("}")

    }

    override def getRules: String = ""

    override def emit(dir: File) {

        // Get devices on this host.
        val localInstances = sp.instances.filter { instance =>
            instance.device != null && instance.device.host == host
        }
        val cpuInstances = localInstances.filter { instance =>
            shouldEmit(instance.device)
        }
        threadIds ++= cpuInstances.zipWithIndex

        // Write include files that we need.
        write("#include \"ScalaPipe.h\"")
        write("#include <pthread.h>")
        write("#include <stdio.h>")
        write("#include <stdlib.h>")
        write("#include <signal.h>")
        write("#include <sys/time.h>")

        write("#define MAX_POLL_COUNT 32")

        // Get streams on this host.
        val localStreams = sp.streams.filter { s =>
            shouldEmit(s.sourceKernel.device) || shouldEmit(s.destKernel.device)
        }

        // Create edge generators for local streams.
        localStreams.foreach { s => addEdgeGenerator(s) }

        // Determine if we need TimeTrial.
        // Note that we need to check every stream on this host.
        val needTimeTrial = sp.streams.filter { s =>
            s.sourceKernel.device.host == host ||
            s.destKernel.device.host == host
        }.exists { s => !s.measures.isEmpty }

        if (needTimeTrial) {
            val threadCount = sp.threadCount(host)
            val bufferSize = sp.parameters.get[Int]('timeTrialBufferSize)
            write("#include \"tta.h\"")
            write(s"static XTTASharedMemory tta($threadCount, $bufferSize);")
            write(s"static XTTAThread *tta_thread = NULL;")
        }

        write("static bool show_extra_stats = false;")

        // Generate code using the edge generators.
        val edgeTop = new ListBuffer[Generator]
        val edgeGlobals = new ListBuffer[Generator]
        val edgeInit = new ListBuffer[Generator]
        val edgeDestroy = new ListBuffer[Generator]
        val edgeStats = new ListBuffer[Generator]
        for ((generator, edgeStreams) <- edgeGenerators) {

            generator.emitCommon()
            edgeTop += generator.extract()

            generator.emitGlobals(edgeStreams)
            edgeGlobals += generator.extract()

            generator.emitInit(edgeStreams)
            edgeInit += generator.extract()

            generator.emitDestroy(edgeStreams)
            edgeDestroy += generator.extract()

            generator.emitStats(edgeStreams)
            edgeStats += generator.extract()

        }

        // Include kernel headers.
        write("extern \"C\" {")
        cpuInstances.foreach { emitKernelHeader(_) }
        write("}")

        // Write the top edge code.
        write(edgeTop)

        // Create kernel structures.
        cpuInstances.foreach { emitKernelStruct(_) }

        write("static unsigned long long start_ticks;")
        write("static struct timeval start_time;")

        // Write the edge globals.
        write(edgeGlobals)

        writeShutdown(cpuInstances, edgeStats)

        // Write the kernel functions.
        val funcs = Seq[Function[KernelInstance, Unit]](
            emitKernelGetFree,
            emitKernelAllocate,
            emitKernelSend,
            emitKernelAvailable,
            emitKernelRead,
            emitKernelRelease,
            emitThread
        )
        cpuInstances.foreach { i =>
            funcs.foreach { f => f.apply(i) }
        }

        // Create the main function.
        write("int main(int argc, char **argv)")
        write("{")
        enter

        // Declare threads.
        for (t <- threadIds.values) {
            write("pthread_t thread" + t + ";")
        }

        write("start_ticks = sp_get_ticks();")
        write("gettimeofday(&start_time, NULL);")

        write("signal(SIGINT, shutdown);")

        // Startup TimeTrial.
        if (needTimeTrial) {

            val ttOutput = sp.parameters.get[String]('timeTrialOutput)
            val ttFile = if (ttOutput == null) "NULL" else ttOutput
            val ttAffinity = sp.parameters.get[Int]('timeTrialAffinity)
            write("tta_thread = new XTTAThread(&tta, " + ttFile + ", " +
                   ttAffinity + ");")

            val sl = localStreams.filter { s =>
                shouldEmit(s.sourceKernel.device) &&
                shouldEmit(s.destKernel.device)
            }
            for (measure <- sl.flatMap { s => s.measures }) {

                // Note that we can use resource 0 here since
                // no threads have been started yet.
                write("tta.SendStart(0, " + measure.stream.index + ", " +
                        measure.getTTAStat + ", " +
                        measure.getTTAMetric + ", " +
                        measure.stream.depth + ", " +
                        "\"" + measure.getName + "\");")

            }

        }

        // Initialize the edges.
        write(edgeInit)

        // Call the kernel init functions.
        cpuInstances.foreach { emitKernelInit(_) }

        write("atexit(showStats);")

        // Start the threads.
        for (t <- threadIds.values) {
            write(s"pthread_create(&thread$t, NULL, run_thread$t, NULL);")
        }
        for (t <- threadIds.values) {
            write(s"pthread_join(thread$t, NULL);")
        }

        // Call the kernel destroy functions.
        cpuInstances.foreach { emitKernelDestroy(_) }

        // Destroy the edges.
        write(edgeDestroy)

        // Shutdown TimeTrial.
        if (needTimeTrial) {
            write("delete tta_thread;")
        }

        write("return 0;")
        leave
        write("}")

        writeFile(dir, s"proc_$host.cpp")

    }

}

