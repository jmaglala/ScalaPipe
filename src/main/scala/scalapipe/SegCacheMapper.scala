package scalapipe


import scala.math
import scalapipe.dsl._
import scalapipe.dsl.SPSegment


private[scalapipe] class SegCacheMapper(
    val _sp: ScalaPipe
    ) extends Mapper(_sp)
{
    // kernel to segment map
    private var kernelToSPSegment = Map[KernelInstance,SPSegment]()

    private[this] def min_buff(s: Stream): Int =
    {
        // We'll just use the max of the two's rates
        val sourceRate: Int = s.sourceKernel.kernel.outputs(0).rate
        val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        
        if (sourceRate > destRate)
        {
            return (sourceRate)
        }
        else
        {
            return (destRate)
        }
    }

    // Greedily Creates segments of size at most M
    private[this] def create_segments()
    {
        val cacheSize = sp.parameters.get[Int]('cache)

        val modules = sp.instances
        val mods = modules.length
        
        //Module gain from source
        var mod_rates = Seq[Int]()
        //Module state size
        var mod_size = Seq[Int]()
        //Minimum buffer sizes
        var edge_size = Seq[Int]()
        //Minimum cost of best segment up to module i
        var mod_sum = Array[Int]()
        //Best segment up to module i
        var mod_ids = Array[Array[Int]]()
        
        //Populate lists with appropriate information
        var kernel_gain = 1
        for (i <- 0 to (modules.length-1)) {
            if (i == 0) {
            kernel_gain *= modules(i).kernelType.configs.filter(c => c.name == "outrate").head.value.long.toInt
            }
            else {
            kernel_gain *= modules(i).kernelType.configs.filter(c => c.name == "outrate").head.value.long.toInt
            kernel_gain /= modules(i).kernelType.configs.filter(c => c.name == "inrate").head.value.long.toInt
            var min_buff_size = modules(i).getInputs(0).parameters.get[Int]('queueDepth)
            edge_size :+= min_buff_size
            //println("minBuf: " + min_buff_size)
            }
            var kernel_state = modules(i).kernelType.configs.filter(c => c.name == "state").head.value.long.toInt
            mod_size :+= kernel_state
            mod_rates :+= kernel_gain

            mod_sum :+= 0
            mod_ids :+= Array[Int]()
        }
        
        
        var k = 0
        var min_c = 0
        for (i <- 0 to (modules.length-1)) {
            if (i == 0) {
            mod_sum(i) = mod_rates(i)
            mod_ids(i) :+= i
            }
            else {
            k = i
            min_c = mod_sum(k-1) + mod_rates(k)
            for (j <- i-1 to 0 by -1) {
                var t_size = 0
                t_size = mod_size.slice(j,i+1).sum
                var mod_start = modules(j)
                var mod_end = modules(i)
                t_size += edge_size.slice(i,j+1).sum
                if (t_size <= cacheSize) {
                    println("j: " + j)
                    var t_cost = mod_rates(i)
                    if (j-1 >= 0) {
                        t_cost = mod_sum(j-1) + mod_rates(i)
                    }
                    if (t_cost < min_c) {
                        min_c = t_cost
                        k = j
                    }
                }
            }
            println("i:" + i)
            mod_sum(i) = min_c
            if (k - 1 >= 0) {
                for (mod_id <- mod_ids(k-1)) {
                    mod_ids(i) :+= mod_id
                }
            }
            mod_ids(i) :+= k
            }
        }
        //println("MOD IDs")
        val seg_starts = mod_ids(mod_ids.length-1)
        var segid = 0
        for (index <- 0 to seg_starts.length - 1) {
            var startKern = seg_starts(index)
            var endKern = 0
            if (index == seg_starts.length-1) {
                endKern = modules.length - 1
            }
            else {
                endKern = seg_starts(index+1) - 1
            }
            var segment = Seq[KernelInstance]()
            for (kernIndex <- startKern to endKern) {
                segment :+= modules(kernIndex)
            }
            segid += 1
            var sps = new SPSegment(segid)
            sps.kernels = segment
            sp.segments :+= sps
        }
        for (segment <- sp.segments) {
            for(k <- segment.kernels)
                {
                    kernelToSPSegment += (k -> segment)
                }
            //segment.foreach(println)
            println()
        }
    }

    // Assigns the minimum buffers to all edges
    private[this] def assign_min_buffers()
    {
        for (s <- sp.streams)
        {
            s.parameters.set('queueDepth, min_buff(s))
        }
    }
        
    // Increases cross-edge buffers to M
    private[this] def assign_cross_buffers()
    {
        println("1")
        // For now, we'll assum 1:1 and do everything at once
        val totalIterations = sp.parameters.get[Int]('iterations)
        val cacheSize = sp.parameters.get[Int]('cache)
        println("2")
        // Cross streams (connect kernels on different segments)
        val crossStreams = sp.streams.filter(s =>(kernelToSPSegment(s.sourceKernel) != kernelToSPSegment(s.destKernel) ))

        for (s <- crossStreams)
        {
            val bytes = s.sourceKernel.kernel.outputs(0).valueType.bytes
            val count = cacheSize / bytes
            s.parameters.set('queueDepth, count)    
        }
    }

    def map() 
    {
        assign_min_buffers()
        create_segments()
        assign_cross_buffers()
    }
    }
