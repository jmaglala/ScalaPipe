package scalapipe.gen

import scalapipe._
import scalapipe.dsl._

import scala.math

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
    private val kernIds = new HashMap[KernelInstance, Int]

    private lazy val openCLEdgeGenerator = new OpenCLEdgeGenerator(sp)
    private lazy val smartFusionEdgeGenerator = new SmartFusionEdgeGenerator(sp)
    private lazy val simulationEdgeGenerator = new SimulationEdgeGenerator(sp)
    private lazy val saturnEdgeGenerator = new SaturnEdgeGenerator(sp)
    private lazy val sockEdgeGenerator = new SockEdgeGenerator(sp, host)
    private lazy val cEdgeGenerator = new CEdgeGenerator
    
    private lazy val cacheScheduleGenerator = new CacheScheduleGenerator(sp)
    
    private def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
    
    private def getHDLEdgeGenerator: EdgeGenerator = {
        val fpga = sp.parameters.get[String]('fpga)
        fpga match {
            case "SmartFusion"      => smartFusionEdgeGenerator
            case "Simulation"       => simulationEdgeGenerator
            case "Saturn"           => saturnEdgeGenerator
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
    
    // Fetch the defined schedulers
    private def getScheduleGenerator : ScheduleGenerator = {
        val sched = sp.parameters.get[String]('sched)
        sched match {
            case "SegCache"               => cacheScheduleGenerator
            case _ =>
                Error.raise(s"unknown scheduler: $sched")
                cacheScheduleGenerator
        }
    }
    //private def getScheduleGenerator : ScheduleGenerator = cacheScheduleGenerator
    
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

    private def emitKernelStruct(kernel	: KernelInstance) {

        val inputCount = kernel.getInputs.size
        val outputCount = kernel.getOutputs.size

        write(s"static struct {")
        enter
        write(s"SPC clock;")
        write(s"jmp_buf env;")
        write(s"volatile uint32_t active_inputs;")
        write(s"SPKernelData data;")
        write(s"struct sp_${kernel.kernelType.name}_data priv;")
        leave
        write(s"} ${kernel.label};")

    }

    private def emitConfig(kernel: KernelInstance,
                           t: ValueType,
                           config: ConfigLiteral): String = {
        val lit = Literal.get(config.default)
        val d = lit match {
            case c: ConfigLiteral =>
                emitConfig(kernel, t, c)
            case _ =>
                val other = kernel.kernelType.getLiteral(lit)
                if (lit.valueType.getClass == t.getClass) {
                    s"($t)$other"
                } else {
                    Error.raise("invalid conversion for config parameter",
                                kernel)
                }
        }
        val param = config.name
        return s"""get_arg<$t>(argc, argv, "-$param", $d)"""
    }

    private def emitKernelInit(kernel: KernelInstance) {

        val instance = kernel.label
        val name = kernel.name //kernel#
        write(s"static void ${instance}_init()")
        write("{")
        enter
            write(s"${instance}.data.get_free = ${instance}_get_free;")
            write(s"${instance}.data.allocate = ${instance}_allocate;")
            write(s"${instance}.data.send = ${instance}_send;")
            write(s"${instance}.data.get_available = ${instance}_get_available;")
            write(s"${instance}.data.read_value = ${instance}_read_value;")
            write(s"${instance}.data.release = ${instance}_release;")
            write(s"sp_${name}_init(&${instance}.priv);")
            
            // Default config options.
            // These are initialized here to allow overrides from
            // the command line.
            for (c <- kernel.kernelType.configs) {
                val name = c.name
                val t = c.valueType.baseType
                val custom = kernel.getConfig(name)
                val value = if (custom != null) custom else c.value
                value match {
                    case cl: ConfigLiteral =>
                        write(s"""$instance.priv.$name = """ +
                            emitConfig(kernel, t, cl) + ";")
                    case l: Literal =>
                        val lit = kernel.kernelType.getLiteral(value)
                        write(s"$instance.priv.$name = ($t)$lit;")
                    case _ => ()
                }
            }

            // Active inputs is set here since it must be
            // initialized before any producer threads start.
            val inPortCount = kernel.getInputs.size
            write(s"$instance.active_inputs = $inPortCount;")

        leave
        write("}")
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

        write(s"static void *${instance}_allocate(int out_port)")
        write(s"{")
        enter
        //write(s"spc_stop(&$instance.clock);")
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
                write(s"tta->LogEvent(${stream.index}, TTA_TYPE_FULL);")
                leave
                write(s"}")
            }
            write(s"break;")
            leave
        }
        write(s"}")
        write(s"if(SPLIKELY(ptr != NULL)) {")
        enter
        //write(s"spc_start(&$instance.clock);")
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

        write(s"static void ${instance}_send(int out_port)")
        write(s"{")
        enter
        val outputCount = kernel.getOutputs.size
        if (outputCount > 0) {
            //write(s"spc_stop(&$instance.clock);")
            write(s"switch(out_port) {")
            for (stream <- kernel.getOutputs) {
                val index = kernel.outputIndex(stream.sourcePort)
                write(s"case $index:")
                enter
                if (stream.usePush) {
                    write(s"tta->LogEvent(${stream.index}, TTA_TYPE_PUSH, " +
                          s"${stream.label}_get_free() == 0);")
                }
                write(s"${stream.label}_send();")
                write(s"break;")
                leave
            }
            write(s"}")
            //write(s"spc_start(&$instance.clock);")
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
        //write(s"spc_stop(&$instance.clock);")
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
        //write(s"spc_start(&$instance.clock);")
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
        write(s"int end_count = 0;")
        //write(s"spc_stop(&$instance.clock);")
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
        write(s"$instance.clock.count += 1;");
        //write(s"spc_start(&$instance.clock);")
        write(s"return ptr;")
        leave
        write(s"}")
        write(s"if(SPUNLIKELY($instance.active_inputs == 0)) {")
        enter
        write(s"if(end_count > 1) {")
        enter
        write(s"longjmp($instance.env, 1);")
        leave
        write(s"}")
        write(s"end_count += 1;")
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
        //write(s"spc_stop(&$instance.clock);")
        write(s"switch(in_port) {")
        for (stream <- kernel.getInputs) {
            val index = kernel.inputIndex(stream.destPort)
            write(s"case $index:")
            enter
            if (stream.usePop) {
                write(s"tta->LogEvent(${stream.index}, TTA_TYPE_POP, " +
                      s"${stream.label}_get_free() == 0);")
            }
            write(s"${stream.label}_release();")
            write(s"break;")
            leave
        }
        write(s"}")
        //write(s"spc_start(&$instance.clock);")
        leave
        write(s"}")

    }

    private def writeShutdown(instances: Traversable[KernelInstance],
                              edgeStats: ListBuffer[Generator]) {

        def writeKernelStats(k: KernelInstance) {
            write(s"ticks = ${k.label}.clock.total_ticks;");
            write(s"reads = ${k.label}.clock.count;")
            write(s"us = (ticks * total_us) / total_ticks;")
            write(s"""fprintf(stderr, \"     ${k.kernelType.name}(""" +
                  s"""${k.label}): %llu ticks, %llu reads, %llu us\\n\", """ +
                  s"""ticks, reads, us);""")
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
        write("unsigned long long reads;")
        write("unsigned long long us;")
        write("unsigned long long total_ticks;")
        write("unsigned long long total_us;")
        write("unsigned long long stop_ticks = sp_get_ticks();")
        write("gettimeofday(&stop_time, NULL);")
        write("total_ticks = stop_ticks - start_ticks;")
        write("total_us = (stop_time.tv_sec - start_time.tv_sec) * 1000000")
        write("         + (stop_time.tv_usec - start_time.tv_usec);")
        write("""fprintf(stderr, "Statistics:\n");""")
        write("fprintf(stderr, \"Total CPU ticks: %llu\\n\", total_ticks);")
        write("fprintf(stderr, \"Total time:      %llu us\\n\", total_us);")
        instances.foreach(writeKernelStats)
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

    private def emitGetArg {

        write("template<typename T>")
        write("static inline T get_arg(int argc, char **argv, " +
              "const char *name, const T d)")
        write("{")
        enter
        write("for(int i = 1; i < argc; i++) {")
        enter
        write("if(!strcmp(argv[i], name) && i + 1 < argc) {")
        enter
        write("std::stringstream str(argv[i + 1]);")
        write("T temp = d;")
        write("str >> temp;")
        write("return temp;")
        leave
        write("}")
        leave
        write("}")
        write("return d;")
        leave
        write("}")

    }

    override def getRules: String = ""

    private def emitMemorySpec(dir: File) {
        if (sp.parameters.get[Boolean]('trace)) {
            val devices = sp.instances.map(_.device).filter { d =>
                shouldEmit(d)
            }.toSet
            for (device <- devices) {
                val gen = new MemorySpecGenerator(sp, device, false)
                gen.emit(dir)
            }
        }
    }
    
    private def emitSegmentFireIterations(segment: SPSegment) {
        if (segment != sp.segments.head && segment != sp.segments.last) {
            write(s"int seg${segment.id}_fire_iterations() {")
            enter
                write(s"if (!std::min(segFireCount[${segment.id-2}]/${segment.input_rate}, (${segment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)} - segFireCount[${segment.id-1}])/${segment.output_rate})) {")
                enter
                    write(s"std::cout << ${"\"This shouldn't happen. Press enter to continue...\""} << std::endl;")
                    write("std::cin.get();")
                leave
                write("}")
                write(s"return std::min(segFireCount[${segment.id-2}]/${segment.input_rate}, (${segment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)} - segFireCount[${segment.id-1}])/${segment.output_rate});")
            leave
            write("}")
        }
    }
    
    // Bool function to return if segment is fireable
    private def emitSegmentFireable(spsegment: SPSegment) {
        val segId = spsegment.id
        
        //Lists the segment's kernels and all the kernels
        val segment = spsegment.kernels
        val localInstances = sp.instances.filter { instance =>
            instance.device != null && instance.device.host == host
        }
        val cpuInstances = localInstances.filter { instance =>
            shouldEmit(instance.device)
        }
        //Queue depths and rates
        var outqueuedepth = 1
        if (spsegment.kernels.last.getOutputs.length != 0)
            outqueuedepth = spsegment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)
        var inqueuedepth = 1
        if (spsegment.kernels.head.getInputs.length != 0)
            inqueuedepth = spsegment.kernels.head.getInputs(0).parameters.get[Int]('queueDepth)
        val segOutRate = spsegment.output_rate
        val segInRate = spsegment.input_rate
        write(s"bool segment${segId}_is_fireable()")
        write("{")
        enter
        
        //If it's the first kernel
        if (segment.head.kernel.inputs.length == 0)
        {
            //If the next segment is on a different processor, use get_available
            if (spsegment.tid != sp.segments(segId).tid)
                write(s"int segmentFireIterations = (${outqueuedepth} - ${cpuInstances(spsegment.kernels.last.index).label}_get_available(0))/${segOutRate};")
            //Otherwise use the segFireCount array
            else
                write(s"int segmentFireIterations = (${outqueuedepth} - segFireCount[${spsegment.id-1}])/${segOutRate};")
            //If the number of iterations is 0 or less than half it's maximum fires, return false
            write(s"if (segmentFireIterations == 0 || segmentFireIterations < ${(outqueuedepth/segOutRate)} * .5)")
            enter
            write("return false;")
            leave

            write("return true;")

        }
        //If it's a middle kernel
        else if (segment.last.kernel.outputs.length != 0)
        {
            //If the previous segment is on a different processor, use get_available
            if (spsegment.tid != sp.segments(segId-2).tid)
                write(s"int maxInputFires = ${cpuInstances(spsegment.kernels.head.index-1).label}_get_available(0)/${segInRate};")
            //Otherwise use the segFireCount array
            else
                write(s"int maxInputFires = segFireCount[${spsegment.id-2}] / ${segInRate};")
            //If the next segment is on a different processor, use get_available
            if (spsegment.tid != sp.segments(segId).tid)
                write(s"int maxOutputFires = (${outqueuedepth} - ${cpuInstances(spsegment.kernels.last.index).label}_get_available(0))/${segOutRate};")
            //Otherwise use the segFireCount array
            else
                write(s"int maxOutputFires = (${outqueuedepth} - segFireCount[${spsegment.id-1}])/${segOutRate};")
                
            //Segment fire iterations is the min of the max output fires and input fires
            write(s"int segmentFireIterations = std::min(maxOutputFires, maxInputFires);")
            //If the number of iterations is 0 or less than half it's maximum fires, return false
            write(s"if (segmentFireIterations == 0 || segmentFireIterations < ${Math.min(inqueuedepth/segInRate,outqueuedepth/segOutRate)} * .5)")
            enter
            write("return false;")
            leave
            
            write("return true;")
        }
        //If it's the last kernel
        else {
            //If the previous segment is on a different processor, use get_available
            if (spsegment.tid != sp.segments(segId-2).tid)
                write(s"int segmentFireIterations = ${cpuInstances(spsegment.kernels.head.index-1).label}_get_available(0)/${segInRate};")
            //Otherwise use the segFireCount array
            else
                write(s"int segmentFireIterations = segFireCount[${spsegment.id-2}] / ${segInRate};")
            //If the number of iterations is 0 or less than half it's maximum fires, return false
            write(s"if (segmentFireIterations == 0 || segmentFireIterations < ${inqueuedepth/segInRate} * .5)")
            enter
            write("return false;")
            leave
            
            write("return true;")

        }
        leave
        write("}")
    }
    
    //Function to fire all kernels in a segment
    private def emitSegmentFire(spsegment: SPSegment) {
        //Set variables for this segment
        val segId = spsegment.id
        val segment = spsegment.kernels
        val localInstances = sp.instances.filter { instance =>
            instance.device != null && instance.device.host == host
        }
        val cpuInstances = localInstances.filter { instance =>
            shouldEmit(instance.device)
        }
        
        write(s"static void fire_segment${segId}()")
        write(s"{")
        enter
        
        //If it's a single kernel in a segment, no control structures needed. Just fire the one kernel
        if (segment.head == segment.last)
        {
            val id = segment.head.index
            
            write(s"sp_${segment.head.name}_run(&${segment.head.label}.priv);")
            leave
            write("}")
            
            return
        }
        
        //Write run_thread variables
        write(s"int fireKernelNum = ${segment.last.index};");
        write("int fireCount = 0;");
        write("int total = 1;")
        write("bool inputEmpty = false;");
        
        //Create while loop to run until the fireCount == requested total
        write("while (inputEmpty == false)");
        write("{");
        enter
        //Switch statement to determine which kernel to fire
        write("switch (fireKernelNum)");
        write("{");
        enter
        //Iterate through 1 to # of kernels
        for (kernel <- segment.reverse) {
            val name = kernel.name //kernel#
            val instance = kernel.label //instance#
            val id = kernel.index //#
            
            //If writing out code for first kernel
            if (id == segment.head.index)
            {
                write(s"case ${id}:")
                enter
                if (sp.parameters.get[Int]('debug) >= 2)
                    write(s"std::cout << 'k' << ${id} << std::endl;")
                //If it has fired the requested total and the output buffer < the next kernel's required input then end
                write(s"if (fireCount >= total && ${cpuInstances(id).label}_get_available(0) < ${cpuInstances(id).label}.priv.inrate)");
                write("{");
                enter
                write("inputEmpty = true;");
                write("break;");
                leave
                write("}");
                //If it has fired the requested total then move onto the next kernel
                write("else if (fireCount >= total)");
                write("{");
                enter
                write("fireKernelNum++;");
                write("break;");
                leave
                write("}");
                //If the current size of output buffer + this kernel's output rate > total size of the output buffer then move onto the next kernel
                write(s"if ((${cpuInstances(id).label}_get_available(0) + ${kernel.label}.priv.outrate) > ${kernel.getOutputs(0).parameters.get[Int]('queueDepth)})");
                write("{");
                enter
                write("fireKernelNum++;");
                leave
                write("}");
                //Otherwise fire kernel and increment fireCount
                write("else")
                write("{")
                enter
                write("fireCount++;")
                write(s"sp_${kernel.name}_run(&${kernel.label}.priv);")
                leave
                write("}")
                write("break;")
                leave
            }
            //If writing out code for the last kernel
            else if (id == segment.last.index) 
            {
                write(s"case ${id}:")
                enter
                if (sp.parameters.get[Int]('debug) >= 2)
                    write(s"std::cout << 'k' << ${id} << std::endl;")
                //If the input buffer < this kernel's input rate, move back a kernel
                write(s"if (${kernel.label}_get_available(0) < ${kernel.label}.priv.inrate)")
                write("{")
                enter
                write("fireKernelNum--;")
                leave
                write("}")
                //Otherwise fire kernel and increment fireCount
                write("else")
                write("{")
                enter
                write(s"sp_${kernel.name}_run(&${kernel.label}.priv);")
                leave
                write("}")
                write("break;")
                leave
            }
            else 
            {
                write(s"case ${id}:")
                enter
                if (sp.parameters.get[Int]('debug) >= 2)
                    write(s"std::cout << 'k' << ${id} << std::endl;")
                //If the output buffer is full or the input buffer < input rate and the next kernel has input then move onto the next kernel
                write(s"if ((${cpuInstances(id).label}_get_available(0) + ${kernel.label}.priv.outrate) > ${kernel.getOutputs(0).parameters.get[Int]('queueDepth)} || ((${kernel.label}_get_available(0) < ${kernel.label}.priv.inrate) && ${cpuInstances(id).label}_get_available(0) > ${cpuInstances(id).label}.priv.inrate))")
                write("{")
                enter
                write("fireKernelNum++;")
                leave
                write("}")
                //If the input buffer < input rate (and the output buffer didn't have input) then move to the previous kernel
                write(s"else if (${kernel.label}_get_available(0) < ${kernel.label}.priv.inrate)")
                write("{")
                enter
                write("fireKernelNum--;")
                leave
                write("}")
                //Otherwise fire the kernel
                write("else")
                write("{")
                enter
                write(s"sp_${kernel.name}_run(&${kernel.label}.priv);")
                leave
                write("}")
                write("break;")
                leave
            } 
        }
        leave
        write("}") // switch(fireKernelNum)
        leave
        write("}") // while (inputEmpty == false)
        leave
        write("}") // static void fire_segment${segId}()
    }
    
    
    private def emitThread(tid: Int)
    {
        //Set variables for this thread ID
        val thread_segments = sp.segments.filter(seg => seg.tid == tid)
        val cpu = sp.parameters.get[Int]('basecpu) + tid
        val localInstances = sp.instances.filter { instance => instance.device != null && instance.device.host == host }
        val cpuInstances = localInstances.filter { instance => shouldEmit(instance.device) }
        
        if (thread_segments.length == 0)
            return
        
        write(s"static void *run_thread${tid}(void *arg)")
        write(s"{")
        enter
        //Affinity 
        write(s"sp_set_affinity(${cpu});")
        //Get total number of iterations from parameter
        var total = sp.parameters.get[Int]('iterations)
        
        write(s"std::vector<Segment*> segmentList;")
        write("std::vector<Kernel*> kernels;")
        for (segment <- thread_segments) {
            
            write(s"for (int i = ${segment.kernels.head.index-1}; i <=${segment.kernels.last.index-1}; i++) {")
            enter
                write("kernels.push_back(modList[i]);")
            leave
            write("}")
            write(s"segmentList.push_back(new Segment(kernels));")
            write("kernels.clear();")
        }
        
        
        //If it's the first thread, then write the total and fireCount to track the fires
        if (tid == 0) {
            write(s"int total = $total;");
            write(s"int fireCount = 0;");
        }
        
        //Create while loop to run until the fireCount == requested total
        write("while (inputEmpty == false)");
        write("{");
        enter
        // Select the segments from this thread's list
        for (i <- thread_segments.length-1 to 0 by -1) 
        {
            val segment = thread_segments(i)
            val segId = segment.id
            
            //If writing out code for first kernel
            if (segId == 1)
            {
                var lastSegOnThread = false
                if (i == thread_segments.length-1) {
                    lastSegOnThread = true
                }
                //If the current size of output buffer + this kernel's output rate > total size of the output buffer then move onto the next kernel
                write(s"if (segmentList[${i}]->isFireable(segFireCount, true, ${lastSegOnThread}) && fireCount < total)");
                write("{");
                enter
                    if (sp.parameters.get[Int]('debug) >= 1)
                        write(s"std::cout << 'p' << ${tid} << ' ' << 's' << ${segId} << std::endl;")
                    write(s"int segmentFireIterations = (${segment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)} - segFireCount[${segment.id-1}])/${segment.output_rate};")

                    //If the threshold is only 1 fire, then add to segFireCount on each fire
                    if (segment.threshold == 1) {
                        write(s"segFireCount[${segment.id-1}] += segmentFireIterations * ${segment.output_rate * segment.threshold};")
                    }
                    //Fire the segment the calculated number of times
                    write(s"for (int i = 0; i < segmentFireIterations; i++) {")
                    enter
                        if (sp.parameters.get[Int]('debug) >= 2)
                            write(s"std::cout << ${"\"FIRING\""} << ' ' << ${segId} << ' ' << segmentFireIterations - i << std::endl;")
                        //If the threshold is > 1, account for the threshold when updating segFireCount
                        if (segment.threshold > 1) {
                            write("static int thresholdCount = 0;")
                            write("thresholdCount++;")
                            write(s"if (thresholdCount == ${segment.threshold}) {")
                            enter
                            write(s"segFireCount[${segment.id-1}] += ${segment.output_rate * segment.threshold};")
                            write("thresholdCount = 0;")
                            leave
                            
                            write("}")   
                        }
                        write("fireCount++;")
                        write(s"segmentList[${i}].fire();")
                    leave
                    write("}")
                    if (sp.parameters.get[Int]('debug) >= 1)
                        write(s"std::cout << 'F' << 'C' << ':' << ' ' << fireCount << std::endl;")
                    write("continue;")
                leave
                write("}")
                
                //If it has fired the requested total then set the global inputEmpty to true and end the program
                write(s"else if (fireCount >= total)")
                write("{")
                enter
                if (sp.parameters.get[Int]('debug) >= 1)
                    write("std::cout << 'd' << 'o' << 'n' << 'e' << std::endl;")
                write("inputEmpty = true;")
                leave
                write("}")  
            }
            //If writing out code for not the first kernel
            else 
            {
                var lastSegOnThread = false
                if (i == thread_segments.length-1) {
                    lastSegOnThread = true
                }
                
                //If the  segment is fireable, fire it
                write(s"if (segmentList[${i}]->isFireable(segFireCount, true, ${lastSegOnThread}))")
                write("{")
                enter
                    if (sp.parameters.get[Int]('debug) >= 1)
                        write(s"std::cout << 'p' << ${tid} << ' ' << 's' << ${segId} << std::endl;")                   
                        
                    //If the kernel isn't the last kernel, use seg_fire_iterations
                    if (segment.kernels.last.getOutputs.length != 0)
                        write(s"int segmentFireIterations = segmentList[${i}]->fireIterations;")
                        
                    //Update the segFireCount array
                    write(s"segFireCount[${segment.id-2}] -= segmentFireIterations * ${segment.input_rate};")
                    //If the threshold is only 1 fire, then add to segFireCount on each fire
                    if (segment.threshold == 1) {
                        write(s"segFireCount[${segment.id-1}] += segmentFireIterations * ${segment.output_rate * segment.threshold};")
                    }
                    //Fire the segment the calculated number of times
                    write(s"for (int i = 0; i < segmentFireIterations; i++) {")
                    enter
                        if (sp.parameters.get[Int]('debug) >= 2)
                            write(s"std::cout << ${"\"FIRING\""} << ' ' << ${segId} << ' ' << segmentFireIterations - i << std::endl;")
                        //If the threshold is > 1, account for the threshold when updating segFireCount
                        if (segment.threshold > 1) {
                            write("static int thresholdCount = 0;")
                            write("thresholdCount++;")
                            write(s"if (thresholdCount == ${segment.threshold}) {")
                            enter
                            write(s"segFireCount[${segment.id-1}] += ${segment.output_rate * segment.threshold};")
                            write("thresholdCount = 0;")
                            leave
                            write("}")
                        }
                        write(s"segmentList[${i}].fire();")
                    leave
                    write("}")
                    
                    write("continue;")
                leave
                write("}")    
            }
            
        }
        leave
        write("}")
        leave
        write("}")
    }
    
    override def emit(dir: File) {

        // Get devices on this host.
        val localInstances = sp.instances.filter { instance =>
            instance.device != null && instance.device.host == host
        }
        val cpuInstances = localInstances.filter { instance =>
            shouldEmit(instance.device)
        }
        
        //threadIds ++= cpuInstances.zipWithIndex // This sets thread per kernel
        val threadIds = sp.parameters.get[Int]('cores) -1// thread per core
        //val threadIds = 0 // Only using one thread
        
        // Write include files that we need.
        write("#include \"ScalaPipe.h\"")
        write("#include \"Kernel.h\"")
        write("#include \"Edge.h\"")
        write("#include \"Segment.h\"")
        write("#include <pthread.h>")
        write("#include <signal.h>")
        write("#include <sstream>")
        write("#include <time.h>")
        write("#include <algorithm>")
        
        //TAKE OUT----------------------------------------------------------
        write("#include <iostream>")

        // Get streams on this host.
        val localStreams = sp.streams.filter { s =>
            shouldEmit(s.sourceKernel.device) || shouldEmit(s.destKernel.device)
        }

        // Create edge generators for local streams.
        //localStreams.foreach { s => addEdgeGenerator(s) }

        // Determine if we need TimeTrial.
        // Note that we need to check every stream on this host.
        /*val needTimeTrial = sp.streams.filter { s =>
            s.sourceKernel.device.host == host ||
            s.destKernel.device.host == host
        }.exists { s => !s.measures.isEmpty }
        */
        /*if (needTimeTrial) {
            write("#include \"TimeTrial.hh\"")
            write("static TimeTrialAgent *tta = NULL;;")
        }*/

        //write("static bool show_extra_stats = false;")

        // Generate code using the edge generators.
        /*val edgeTop = new ListBuffer[Generator]
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
        */
        // Include kernel headers.
        write("extern \"C\" {")
        cpuInstances.foreach(emitKernelHeader)
        write("}")

        write("bool inputEmpty = false;");
        write(s"int segFireCount[${sp.segments.length}];")
        //write(s"Kernel ** modList = new Kernel *[${cpuInstances.length}];")
        write(s"std::vector<Kernel*> modList;")
        
        // Write the top edge code.
        //write(edgeTop)

        // Create kernel structures.
        //cpuInstances.foreach(emitKernelStruct)

        
        write("static unsigned long long start_ticks;")
        write("static struct timeval start_time;")

        // Write the edge globals.
        //write(edgeGlobals)

        //writeShutdown(cpuInstances, edgeStats)

        // Write the kernel functions.
        /*val funcs = Seq[Function[KernelInstance, Unit]](
            emitKernelGetFree,
            emitKernelAllocate,
            emitKernelSend,
            emitKernelAvailable,
            emitKernelRead,
            emitKernelRelease,
            emitKernelInit
            //emitThread
        )
        cpuInstances.foreach { i =>
            funcs.foreach { f => f.apply(i) }
        }
        */
        // Write the segment functions
        /*for (segment <- sp.segments)
        {
            emitSegmentFireIterations(segment)
            emitSegmentFireable(segment)
            emitSegmentFire(segment)
        }*/
        
        // Thread functions
        for (t <- 0 to threadIds) {
            emitThread(t)
        }
        // Create the "get_arg"         function.
        emitGetArg

        // Create the main function.
        write("int main(int argc, char **argv)")
        write("{")
        enter
        //val initialSourceBuffer : Int = (sp.segments.head.kernels.head.getOutputs(0).parameters.get[Int]('queueDepth)/sp.segments.head.output_rate).toInt * sp.segments.head.output_rate.toInt
        //write(s"segFireCount[0] = ${0};")
        
        // Declare threads.
        for (t <- 0 to threadIds) {
            write(s"pthread_t thread$t;")
        }

        write("start_ticks = sp_get_ticks();")
        write("gettimeofday(&start_time, NULL);")

        write("signal(SIGINT, shutdown);")

        // Startup TimeTrial.
        /*
        if (needTimeTrial) {
            val ttOutput = sp.parameters.get[String]('timeTrialOutput)
            val ttFile = if (ttOutput == null) {
                    "NULL"
                } else {
                    "\"" + ttOutput + "\""
                }
            val ttSize = sp.parameters.get[Int]('timeTrialBufferSize)
            val ttAffinity = sp.parameters.get[Int]('timeTrialAffinity)
            write(s"tta = new TimeTrialAgent($ttSize, $ttAffinity, $ttFile);")

            val sl = localStreams.filter { s =>
                shouldEmit(s.sourceKernel.device) &&
                shouldEmit(s.destKernel.device)
            }.flatMap(_.measures).foreach { measure =>
                val id = measure.stream.index
                val stat = measure.getTTAStat
                val metric = measure.getTTAMetric
                val depth = measure.stream.parameters.get[Int]('queueDepth)
                val name = "\"" + measure.getName + "\""
                write(s"tta->SendStart($id, $stat, $metric, $depth, $name);")
            }

        }*/

        for (kernel <- cpuInstances) {
            val instance = kernel.label //instance#
            val kname = kernel.name
            var in = -1
            if (kernel.kernel.inputs.length > 0)
                in = kernel.kernel.inputs(0).rate
            var out = -1
            if (kernel.kernel.outputs.length > 0)
                out = kernel.kernel.outputs(0).rate
            val state = kernel.kernelType.configs.filter(c => c.name == "state").head.value.long.toInt
            val runtime = kernel.kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt

            write(s"modList.append(new ${kname}(${in},${out},${state},${runtime}))")

        }
        
        // Initialize the edges.
        //write(edgeInit)
        write(s"std::vector<Edge*> edges;")
        for (s <- sp.streams)
        {
            val source = s.sourceKernel.index
            val dest = s.destKernel.index
            val depth = s.parameters.get[Int]('queueDepth)
            write(s"edges.append(new Edge(${depth},modList[${source}],modList[${dest}]))")
        }
        write("atexit(showStats);")

        // Start the threads.
        write("struct timespec start, end, diff;")
        write("clock_gettime(CLOCK_MONOTONIC, &start);")
        for (t <- 0 to threadIds) {
            write(s"pthread_create(&thread$t, NULL, &run_thread$t, NULL);")
        }
        
        
        
        for (t <- 0 to threadIds) {
            write(s"pthread_join(thread$t, NULL);")
        }

        write("clock_gettime(CLOCK_MONOTONIC, &end);")
        write("elapsed(&diff,&end,&start);")
        write(s"fprintf(stdout, ${'\"'}%ld.%.9ld${"\\n\""},diff.tv_sec,diff.tv_nsec);")
        // Destroy the edges.
        //write(edgeDestroy)

        // Shutdown TimeTrial.
        /*if (needTimeTrial) {
            write("delete tta;")
        }*/

        write("return 0;")
        leave
        write("}")

        writeFile(dir, s"proc_$host.cpp")
        emitMemorySpec(dir)

    }

}
