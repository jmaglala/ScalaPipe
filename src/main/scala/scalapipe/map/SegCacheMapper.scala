package scalapipe.map


import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

// Helpers to dertmine cache size


private[scalapipe] class SegCacheMapper(
    val _sp: ScalaPipe
    ) extends Mapper(_sp) with AugmentBuffer
{

    // Greedily Creates segments of size at most M
    def create_segments() : Unit = 
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
                mod_sum(i) = min_c
                if (k - 1 >= 0) {
                    for (mod_id <- mod_ids(k-1)) {
                        mod_ids(i) :+= mod_id
                    }
                }
                mod_ids(i) :+= k
            }
        }

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
                    if (sp.parameters.get[Int]('debug) >= 2)
                        print(k)
                    kernelToSPSegment += (k -> segment)
                }
            //segment.foreach(println)
            segment.initVariables()
            if (sp.parameters.get[Int]('debug) >= 2)
                println('\n' + "in:" + segment.input_rate + " out:" + segment.output_rate + " threshold:" + segment.threshold + " amp:" + segment.amplification + " run:" + segment.runtime + " state:" + segment.state + '\n')
        }
    }

    def assign_segments_to_cores() : Unit = {
        val segPerCore = sp.segments.length/sp.parameters.get[Int]('cores)
        val extraSegs = sp.segments.length%sp.parameters.get[Int]('cores)
        var segNum = 0
        if (sp.parameters.get[Int]('debug) >= 2) {
            println("ASSIGNING SEGS TO CORES")
            println("Min SegPerCore: " + segPerCore)
        }
        for (i <- 0 to (sp.parameters.get[Int]('cores)-1)) {
            if (sp.parameters.get[Int]('debug) >= 2)
                print("Core " + i + ": ")
            for (j <- 1 to segPerCore) {
                if (sp.parameters.get[Int]('debug) >= 2)
                    print(segNum + " ")
                sp.segments(segNum).tid = i
                segNum += 1
            }
            if (i < (extraSegs)) {
                if (sp.parameters.get[Int]('debug) >= 2)
                    print(segNum + " ")
                sp.segments(segNum).tid = i
                segNum += 1
            }
            if (sp.parameters.get[Int]('debug) >= 2)
                println()
        }
        if (sp.parameters.get[Int]('debug) >= 2)
            println("DONE ASSIGNING SEGS TO CORES")
    }
    
    
}
