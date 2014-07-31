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
    
    private def emitThread(tid: Int)
    {
        //Set variables for this thread ID
        val thread_segments = sp.segments.filter(seg => seg.tid == tid)
        val cpu = sp.parameters.get[Int]('basecpu) + tid
        val localInstances = sp.instances.filter { instance => instance.device != null && instance.device.host == host }
        val cpuInstances = localInstances.filter { instance => shouldEmit(instance.device) }
        
        write(s"static void *run_thread${tid}(void *arg)")
        write(s"{")
        enter
        //Affinity 
        write(s"sp_set_affinity(${cpu});")
        //Get total number of iterations from parameter
        var total = sp.parameters.get[Int]('iterations)
        
        
        //If it's the first thread, then write the total and fireCount to track the fires
        if (tid == 0) {
            write(s"int total = $total;");
            write(s"int fireCount = 0;");
        }
        
        //Create while loop to run until the fireCount == requested total
        write("while (inputEmpty == false)");
        write("{");
        enter
        if (thread_segments.length > 0) {
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
                    write(s"if (segmentList[${segId-1}]->isFireable() && fireCount < total)");
                    write("{");
                    enter
                        // First load the segment
                        write(s"segmentList[${segId-1}]->load();")
                        
                        if (sp.parameters.get[Int]('debug) >= 1)
                            write(s"std::cout << 'p' << ${tid} << ' ' << 's' << ${segId} << std::endl;")
                        write(s"int segmentFireIterations = segmentList[${segId-1}]->fireIterations();")

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
                            write(s"segmentList[${segId-1}]->fire();")
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
                    write(s"if (segmentList[${segId-1}]->isFireable())")
                    write("{")
                    enter
                        if (sp.parameters.get[Int]('debug) >= 1)
                            write(s"std::cout << 'p' << ${tid} << ' ' << 's' << ${segId} << std::endl;")                   
                            
                        write(s"int segmentFireIterations = segmentList[${segId-1}]->fireIterations();")
                            
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
                            write(s"segmentList[${segId-1}]->fire();")
                        leave
                        write("}")
                        
                        write("continue;")
                    leave
                    write("}")    
                }
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
        write("#include \"Kernel.cpp\"")
        write("#include \"Edge.cpp\"")
        write("#include \"Segment.cpp\"")
        write("#include \"SPQ.cpp\"")
        write("#include \"TSPQ.cpp\"")
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
        write(s"std::vector<Kernel*> modList;")
        write(s"std::vector<Segment*> segmentList;")
        write(s"std::vector<Edge*> edges;")
        
        // Write the top edge code.
        //write(edgeTop)

        // Create kernel structures.
        //cpuInstances.foreach(emitKernelStruct)

        
        write("static unsigned long long start_ticks;")
        write("static struct timeval start_time;")


        //writeShutdown(cpuInstances, edgeStats)
        
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
        
        // Declare threads.
        for (t <- 0 to threadIds) {
            write(s"pthread_t thread$t;")
        }

        write("start_ticks = sp_get_ticks();")
        write("gettimeofday(&start_time, NULL);")

        //write("signal(SIGINT, shutdown);")

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

            write(s"modList.push_back(new ${kname}(${in},${out},${state},${runtime}));")

        }
        
        // Initialize the edges.
        //write(edgeInit)
        for (s <- sp.streams)
        {
            val source = s.sourceKernel.index - 1
            val dest = s.destKernel.index - 1
            val depth = s.parameters.get[Int]('queueDepth)
            val vtype = s.valueType
            val cross = s.parameters.get[Boolean]('crossedge)
            if (cross)
                write(s"edges.push_back(new TSPQ(${depth},sizeof(${vtype})));")
            else
                write(s"edges.push_back(new SPQ(${depth},sizeof(${vtype})));")
            // Link the edges to the kernels
            write(s"modList[${source}]->outputs.push_back(edges.back());")
            write(s"modList[${dest}]->inputs.push_back(edges.back());")
        }
        //write("atexit(showStats);")

        write("std::vector<Kernel*> kernels;")
        for (segment <- sp.segments) {
            val segId = segment.id
            write(s"for (int i = ${segment.kernels.head.index-1}; i <=${segment.kernels.last.index-1}; i++)")
            enter
                write("kernels.push_back(modList[i]);")
            leave
            write(s"segmentList.push_back(new Segment(${segment.id-1}, kernels, segFireCount));")
            write("kernels.clear();")
            if (segment != sp.segments.head) {
                write(s"segmentList[${segId-1}]->prev_seg = segmentList[${segId-2}];")
                write(s"segmentList[${segId-2}]->next_seg = segmentList[${segId-1}];")
            }
            if (segment != sp.segments.last) {
            }
        }
        
        
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
