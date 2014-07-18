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
        
        sp.segments = int_best_partition(0,mods-1)
        
        for (segment <- sp.segments) {
            print(segment.id + ") ")
            for(k <- segment.kernels)
                {
                    if (sp.parameters.get[Int]('debug) >= 2)
                        print(k)
                    kernelToSPSegment += (k -> segment)
                }
            //segment.foreach(println)
            segment.initVariables()
            if (sp.parameters.get[Int]('debug) >= 2)
            {
                println('\n' + "in:" + segment.input_rate + " out:" + segment.output_rate + " threshold:" + segment.threshold + " amp:" + segment.amplification + " run:" + segment.runtime + " state:" + segment.state + '\n')
            }
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
