package scalapipe.map

import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

private[scalapipe] class EvenSegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp) with AugmentBuffer
{
    private val numSegs = sp.parameters.get[Int]('schedparam)
    
    // Greedily Creates segments of size at most M
    def create_segments() : Unit = 
    {
        var segId = 0
        var spseg = new SPSegment(segId)
        segId += 1
        var seg = Seq[KernelInstance]()
        var size: Int = numSegs
        println(size)
        // Put everything into one segment
        var i: Int = 0
        for (k <- sp.instances) 
        {
            seg :+= k
            kernelToSPSegment += (k -> spseg)
	    
            i += 1
            if (i > 0 && i % size == 0)
            {
                spseg.kernels = seg
                sp.segments :+= spseg
                spseg = new SPSegment(segId)
                seg = Seq[KernelInstance]()
                segId += 1
            }

        }
        if (seg.length > 0) {
            spseg.kernels = seg
            sp.segments :+= spseg
        }
        for (segment <- sp.segments)
        {
            for(k <- segment.kernels)
            {
                kernelToSPSegment += (k -> segment)
            }
        }
        //sp.segments.foreach(println)
    }
        
    def assign_segments_to_cores() : Unit = {
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
    
}
