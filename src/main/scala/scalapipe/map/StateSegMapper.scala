package scalapipe.map

import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

private[scalapipe] class StateSegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp) with AugmentBuffer
{
    def create_segments() : Unit = {
        //Make variables to control loop
        val cacheSize = sp.parameters.get[Int]('cache)
        val modules = sp.instances
        val modsLen = modules.length
        var totalSegState = 0
        var segStart = 0
        var segid = 0
        
        //Iterate over all modules
        for (i <- 0 to modsLen-1) {
            //Store current segment state size
            var currentSegState = modules(i).kernelType.configs.filter(c => c.name == "state").head.value.long.toInt
            //If the total + the current state size > cache or it's the last kernel
            //then create a segment
            if (totalSegState + currentSegState > cacheSize || i == modsLen-1) {
                totalSegState -= modules(i-1).getOutputs(0).parameters.get[Int]('queueDepth)
                segid += 1
                var sps = new SPSegment(segid)
                var segment = Seq[KernelInstance]()
                //Add all kernels from the last place stopped up until the kernel before 
                //the current kernel
                for (i <- segStart to (i-1)) {
                    segment :+= modules(i)
                }
                //If it's the last kernel, add it to the segment too
                if (i == modsLen-1)
                    segment :+= modules(i)
                sps.kernels = segment
                
                //DEBUG
                print(totalSegState + ": ")
                sps.kernels.foreach(print)
                println()
                
                sp.segments :+= sps
                totalSegState = 0
                segStart = i
            }
            //Otherwise add the kernel's state size to the total and move on
            else {
                totalSegState += currentSegState
                totalSegState += modules(i).getOutputs(0).parameters.get[Int]('queueDepth)
            }
        }
        
        //Stuff for the AugmentBuffer
        for (segment <- sp.segments) {
            for(k <- segment.kernels)
                {
                    println(k)
                    kernelToSPSegment += (k -> segment)
                }
            segment.initVariables()
            //DEBUG
            //segment.foreach(println)
            //println()
        }
    }
    def assign_segments_to_cores() : Unit = {
        //Assign segments evenly to cores
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
        for (seg <- sp.segments) {
            print(seg.tid)
        }
    }
}