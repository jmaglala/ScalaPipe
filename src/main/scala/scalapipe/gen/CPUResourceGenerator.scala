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
            write(s"sp_${name}_init(&${instance}.priv);");
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

    // Segment Functions
    private def emitSegmentFieable(spsegment: SPSegment) {
        val segment = spsegment.kernels
        val segId = spsegment.id
        write(s"bool segment${segId}_is_fireable()")
        write("{")
        enter
        
        //Get output rates and output buffer sizes
        var segAmplification = 1
        for (kernel <- segment) {
            if (kernel != segment.head) {
                segAmplification = segAmplification / kernel.kernel.inputs(0).rate
            }
            if (kernel.kernel.outputs.length != 0) {
                segAmplification = segAmplification * kernel.kernel.outputs(0).rate
            }
            else {
                segAmplification = -1
            }
        }
        
        //If first kernel is true
        if (segment.head.kernel.inputs.length == 0)
        {
            write("return true;")
        }
        else if (segment.last.kernel.outputs.length == 0)
        {
            write(s"return ${segment.head.label}_get_available(0) == ${segment.head.getInputs(0).parameters.get[Int]('queueDepth)};")
        }
        else
        {
            write(s"return ${segment.head.label}_get_available(0) == ${segment.head.getInputs(0).parameters.get[Int]('queueDepth)} || ${segment.head.label}_get_available(0)/${segment.head.kernel.inputs(0).rate} * ${segAmplification} >= ${segment.last.getOutputs(0).parameters.get[Int]('queueDepth)};")
        }
        leave
        write("}")
    }
    
    private def emitSegmentFire(spsegment: SPSegment) {
        // Get devices on this host.
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
        //Write kernel inits and rates
        for (kernel <- segment) {
            //getScheduleGenerator.emitThread(t)
            val name = kernel.name //kernel#
            val instance = kernel.label //instance#
            val id = kernel.index //#
            if (kernel.index != segment.last.index) {
                val outputRate = kernel.kernel.outputs(0).rate.toInt;
                write(s"int ${name}_out_rate = ${outputRate};");
                
                val bufferSize = kernel.getOutputs(0).parameters.get[Int]('queueDepth)
                write(s"int ${name}_out_buf_size = ${bufferSize};");
            }
            if (kernel.index != segment.head.index) {
                val inputRate = kernel.kernel.inputs(0).rate;
                write(s"int ${name}_in_rate = ${inputRate};");
            }
        }
        
        if (segment.head == segment.last)
        {
            val id = segment.head.index
            
            write(s"sp_${segment.head.name}_run(&${segment.head.label}.priv);")
            leave
            write("}")
            
            return
        }
        
        //Write run_thread variables
        write(s"int fireKernelNum = ${segment.head.index};");
        write("int fireCount = 0;");
        write("bool inputEmpty = false;");
        
        //Get total number of iterations from parameter
        //var total = sp.parameters.get[Int]('iterations)
        var total = 1;
        write(s"int total = $total;");
        
        //Create while loop to run until the fireCount == requested total
        write("while (inputEmpty == false)");
        write("{");
        enter
        //Switch statement to determine which kernel to fire
        write("switch (fireKernelNum)");
        write("{");
        enter
        //Iterate through 1 to # of kernels
        for (kernel <- segment) {
            //getScheduleGenerator.emitThread(t)
            val name = kernel.name //kernel#
            val instance = kernel.label //instance#
            val id = kernel.index //#
            
            
            
            //If writing out code for first kernel
            if (id == segment.head.index)
            {
                write(s"case ${id}:")
                enter
                //If it has fired the requested total and the output buffer < the next kernel's required input then end
                write(s"if (fireCount >= total && ${cpuInstances(id).label}_get_available(0) < ${cpuInstances(id).name}_in_rate)");
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
                write(s"if ((${cpuInstances(id).label}_get_available(0) + ${kernel.name}_out_rate) > ${kernel.name}_out_buf_size)");
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
                //If the input buffer < this kernel's input rate, move back a kernel
                write(s"if (${kernel.label}_get_available(0) < ${kernel.name}_in_rate)")
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
                //If 
                write(s"if ((${cpuInstances(id).label}_get_available(0) + ${kernel.name}_out_rate) > ${kernel.name}_out_buf_size || ((${kernel.label}_get_available(0) < ${kernel.name}_in_rate) && ${cpuInstances(id).label}_get_available(0) > ${cpuInstances(id).name}_in_rate))")
                write("{")
                enter
                write("fireKernelNum++;")
                leave
                write("}")
                write(s"else if (${kernel.label}_get_available(0) < ${kernel.name}_in_rate)")
                write("{")
                enter
                write("fireKernelNum--;")
                leave
                write("}")
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
        val thread_segments = sp.segments.filter(seg => seg.tid == tid)
    
        write(s"static void *run_thread${tid}(void *arg)")
        write(s"{")
        enter
        
        //Write run_thread variables
        write("int fireCount = 0;");
        write("bool inputEmpty = false;");
        
        //Get total number of iterations from parameter
        var total = sp.parameters.get[Int]('iterations)
        write(s"int total = $total;");
        
        //Create while loop to run until the fireCount == requested total
        write("while (inputEmpty == false)");
        write("{");
        enter
        
        // Select the segments from this thread's list
        for (segment <- thread_segments.reverse) 
        {
            val segId = segment.id
            
            var segmentFireIterations : Double = -1
            
            //Calculates max fires based on gain and output buffer size
            var maxOutputFires : Double = 1
            
            /*for (kernel <- segment.kernels) {
                if (kernel != segment.kernels.head) {
                    maxOutputFires = maxOutputFires / kernel.kernel.inputs(0).rate
                }
                if (kernel.kernel.outputs.length != 0) {
                    maxOutputFires = maxOutputFires * kernel.kernel.outputs(0).rate
                }
                else {
                    maxOutputFires = -1
                }
                println(s"${kernel.name} : ${maxOutputFires}")
            }
            
            //If the segment has outputs, finish calculating max # of firing iterations
            if (maxOutputFires != -1) {
                maxOutputFires = segment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)/maxOutputFires
            }*/
            
            // First segment
            if (segment.kernels.head.kernel.inputs.length == 0)
            {
                val queuedepth = segment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)
                val outrate = segment.kernels.last.kernel.outputs(0).rate
                segmentFireIterations = queuedepth / outrate
            }
            // Last segment
            else if(segment.kernels.last.kernel.outputs.length == 0)
            {       
                val queuedepth = segment.kernels.head.getInputs(0).parameters.get[Int]('queueDepth)
                val inrate = segment.kernels.head.kernel.inputs(0).rate
                segmentFireIterations = queuedepth / inrate
            }
            else
            {
                val inqueuedepth = segment.kernels.head.getInputs(0).parameters.get[Int]('queueDepth)
                val outqueuedepth = segment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)
                val maxInputFires = inqueuedepth / segment.kernels.head.kernel.inputs(0).rate
                val maxOutputFires = outqueuedepth / segment.kernels.last.kernel.outputs(0).rate
                segmentFireIterations = Math.min(maxInputFires, maxOutputFires)
            }
            
            
            
            
            //Calculate max # of firing iterations if the segment has inputs
//             var maxInputFires = -1
//             if (segment.kernels.head.kernel.inputs.length != 0) {
//                 maxInputFires = segment.kernels.head.getInputs(0).parameters.get[Int]('queueDepth)/segment.kernels.head.kernel.inputs(0).rate
//             }
//             
//             if (maxInputFires == -1) {
//                 segmentFireIterations = segment.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)/maxOutputFires
//             }
//             else if (maxOutputFires == -1) {
//                 segmentFireIterations = maxInputFires
//             }
//             else {
//                 segmentFireIterations = Math.min(maxInputFires, maxOutputFires)
//             }
            
            //println(segId + " " + segmentFireIterations + " " + maxInputFires + " " + maxOutputFires)
            
            //If writing out code for first kernel
            if (segId == 1)
            {
                //If the current size of output buffer + this kernel's output rate > total size of the output buffer then move onto the next kernel
                write(s"if (segment${segId}_is_fireable() && fireCount < total)");
                write("{");
                enter
                    write(s"for (int i = 0; i < ${segmentFireIterations}; i++) {")
                    enter
                        write("fireCount++;")
                        write(s"fire_segment${segId}();")
                        write("}")
                    leave
                leave
                write("}");
                
                //If it has fired the requested total and the output buffer < the next kernel's required input then end
                write(s"else if (fireCount >= total)");
                write("{");
                enter
                write("inputEmpty = true;");
                leave
                write("}");  
            }
            //If writing out code for the last kernel
            else 
            {
                //If the input buffer < this kernel's input rate, move back a kernel
                write(s"if (segment${segId}_is_fireable())")
                write("{")
                enter
                    write(s"for (int i = 0; i < ${segmentFireIterations}; i++) {")
                    enter
                        write(s"fire_segment${segId}();")
                        write("}")
                    leave
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
        write("#include <pthread.h>")
        write("#include <signal.h>")
        write("#include <sstream>")
        
        //TAKE OUT----------------------------------------------------------
        write("#include <iostream>")

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
            write("#include \"TimeTrial.hh\"")
            write("static TimeTrialAgent *tta = NULL;;")
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
        cpuInstances.foreach(emitKernelHeader)
        write("}")

        // Write the top edge code.
        write(edgeTop)

        // Create kernel structures.
        cpuInstances.foreach(emitKernelStruct)

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
            emitKernelInit
            //emitThread
        )
        cpuInstances.foreach { i =>
            funcs.foreach { f => f.apply(i) }
        }
        
        // Write the segment functions
        for (segment <- sp.segments)
        {
            emitSegmentFieable(segment)
            emitSegmentFire(segment)
        }
        
        // Thread functions
        for (t <- 0 to threadIds) {
            emitThread(t)
        }
        // Create the "get_arg" function.
        emitGetArg

        // Create the main function.
        write("int main(int argc, char **argv)")
        write("{")
        enter

        // Declare threads.
        for (t <- 0 to threadIds) {
            write(s"pthread_t thread$t;")
        }

        write("start_ticks = sp_get_ticks();")
        write("gettimeofday(&start_time, NULL);")

        write("signal(SIGINT, shutdown);")

        // Startup TimeTrial.
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

        }

        for (kernel <- cpuInstances) {
		val instance = kernel.label //instance#
		write(s"${instance}_init();")
	    }
        
        // Call the kernel init functions.
        //cpuInstances.foreach(emitKernelInit)
        
        // Initialize the edges.
        write(edgeInit)
        
        write("atexit(showStats);")

        // Start the threads.
        for (t <- 0 to threadIds) {
            write(s"pthread_create(&thread$t, NULL, &run_thread$t, NULL);")
        }
        for (t <- 0 to threadIds) {
            write(s"pthread_join(thread$t, NULL);")
        }

        // Destroy the edges.
        write(edgeDestroy)

        // Shutdown TimeTrial.
        if (needTimeTrial) {
            write("delete tta;")
        }

        write("return 0;")
        leave
        write("}")

        writeFile(dir, s"proc_$host.cpp")
        emitMemorySpec(dir)

    }

}
