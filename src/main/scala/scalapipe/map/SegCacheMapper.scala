package scalapipe.map


import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

// Helpers to dertmine cache size


private[scalapipe] class SegCacheMapper(
    val _sp: ScalaPipe
    ) extends Mapper(_sp)
{
    // kernel to segment map
    private var kernelToSPSegment = Map[KernelInstance,SPSegment]()
    
    private[this] def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
    private[this] def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)

    private[this] def cross_buff(s: Stream): Int = 
    {
        val sourceRate: Int = s.sourceKernel.kernel.outputs(0).rate
        val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        
        // For now we're assuming the same size data... which is right?
        val bytes = s.sourceKernel.kernel.outputs(0).valueType.bytes
        val cacheSize = sp.parameters.get[Int]('cache) / bytes
        val t_lcm = lcm(sourceRate,destRate) 
        if (cacheSize % t_lcm == 0) return cacheSize
        else return ((cacheSize / t_lcm) + 1) * t_lcm
    }

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
            if (i == 0) 
            {
                mod_sum(i) = mod_rates(i)
                mod_ids(i) :+= i
            }
            else 
            {
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

    private[this] def assign_segments_to_core() {
        val segPerCore = sp.segments.length/sp.parameters.get[Int]('cores)
        val extraSegs = sp.segments.length%sp.parameters.get[Int]('cores)
        var segNum = 0
        print("SegPerCore: " + segPerCore + " - ")
        for (i <- 0 to (sp.parameters.get[Int]('cores)-1)) {
            for (j <- 1 to segPerCore) {
                println(segNum + " " + i)
                sp.segments(segNum).tid = i
                segNum += 1
            }
            if (i < (extraSegs)) {
                println(segNum + " " + i)
                sp.segments(segNum).tid = i
                segNum += 1
            }
        }
        for (segIndex <- segNum to (sp.segments.length - 1)) {
            
        }
        for (seg <- sp.segments) {
            print(seg.tid)
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
        // Cross streams (connect kernels on different segments)
        val crossStreams = sp.streams.filter(s =>(kernelToSPSegment(s.sourceKernel) != kernelToSPSegment(s.destKernel) ))

        for (s <- crossStreams)
        {
            val count = cross_buff(s)
            s.parameters.set('queueDepth, count)    
        }
    }

    def map() 
    {
        assign_min_buffers()
        create_segments()
        assign_segments_to_core()
        assign_cross_buffers()
    }
}
