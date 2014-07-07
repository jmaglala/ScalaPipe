package scalapipe.map

import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

private[scalapipe] class RuntimeSegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp) with MinBufResize
{
    def create_segments() : Unit = {
        val modules = sp.instances
        val modsLen = modules.length
        val numOfCores = sp.parameters.get[Int]('cores)
        var totalRuntime = 0
        
        //If there are less modules than cores, give each module its own segment and core
        if (modsLen < numOfCores) {
            for (i <- 1 to modsLen) {
                var sps = new SPSegment(i)
                sps.kernels :+= modules(i-1)
                print(i + " ")
                sps.kernels.foreach(println)
                sps.initVariables()
                sp.segments :+= sps
            }
            return
        }
        
        //Calculate total runtime
        for (kernel <- modules)
            totalRuntime += kernel.kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
        
        //Setup variables to control loop
        val runtimeAvg = totalRuntime/numOfCores
        var maxRuntime = runtimeAvg
        var kernelIndex = 0
        
        if (sp.parameters.get[Int]('debug) >= 2) {
            println("Total Runtime: " + totalRuntime)
            println("Avg: " + (totalRuntime/numOfCores))
        }
        
        //Create segment for each core
        for (i <- 1 to numOfCores) {
            if (sp.parameters.get[Int]('debug) >= 2)
                println("MR: " + maxRuntime)
            
            //Setup segment and variables to control while loop
            var sps = new SPSegment(i)
            var kernelRuntime = modules(kernelIndex).kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
            var segmentRuntimeTotal = kernelRuntime
            //While the index is in range and the current total runtime + the next kernel won't exceed the
            //max desired runtime, add the kernel and update the variables
            while (kernelIndex < modules.length && segmentRuntimeTotal <= maxRuntime) {
                sps.kernels :+= modules(kernelIndex)
                kernelIndex += 1
                if (kernelIndex != modules.length) {
                    kernelRuntime = modules(kernelIndex).kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
                    segmentRuntimeTotal += kernelRuntime
                }
            }
            segmentRuntimeTotal -= kernelRuntime
            println("SRT: " + segmentRuntimeTotal + " " + "MR: " + maxRuntime)

            //If there is leftover runtime from the previous segment, include it in the next segment
            maxRuntime = maxRuntime + (maxRuntime - segmentRuntimeTotal)

            sp.segments :+= sps
        }
        for (segment <- sp.segments) {
            for(k <- segment.kernels) {
                if (sp.parameters.get[Int]('debug) >= 2)
                    print(k)
                kernelToSPSegment += (k -> segment)
            }
            segment.initVariables()
            if (sp.parameters.get[Int]('debug) >= 2)
                println('\n' + "in:" + segment.input_rate + " out:" + segment.output_rate + " threshold:" + segment.threshold + " amp:" + segment.amplification + " run:" + segment.runtime + " state:" + segment.state + '\n')
        }
    }
    
    def assign_segments_to_cores() : Unit = {
        //Since there are as many segments as cores, assign segment i to core i
        for (i <- 0 to sp.segments.length-1)
            sp.segments(i).tid = i
    }
}