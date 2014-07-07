package scalapipe.map

import scala.math
import util.Random.nextInt
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

private[scalapipe] class GainNSegMapper(
    val _sp: ScalaPipe, val _nEdges: Int
) extends Mapper(_sp) with AugmentBuffer
{
    def create_segments() : Unit = {
        val modules = sp.instances
        val modsLen = modules.length
        var kernGains = Array[Int]()
        var rand = Array[Int]()
        var finalEdges = Array[Int]()
        
        var gainTotal = 1
        //Build kernlGains to hold all gains in Array
        for (kernel <- modules) {
            gainTotal *= kernel.kernelType.configs.filter(c => c.name == "outrate").head.value.long.toInt
            if (kernel != modules.head)
                gainTotal /= kernel.kernelType.configs.filter(c => c.name == "inrate").head.value.long.toInt
            if (kernel != modules.last)
                kernGains :+= gainTotal
        }
        
        //Setup vars for loop control
        var edgeSelect = _nEdges
        var minGain = kernGains.min
        
        while (edgeSelect > 0) {
            var tempEdges = Array[Int]()
            if (sp.parameters.get[Int]('debug) >= 2)
                println("min: " + minGain)
            
            //Add all minimum gains to an tempEdges
            if (sp.parameters.get[Int]('debug) >= 2)
                println("adding min gain edges")
            for (i <- 0 to kernGains.length-1) {
                if (kernGains(i) == minGain) {
                    tempEdges :+= i
                }
            }
            if (sp.parameters.get[Int]('debug) >= 2)
                println("min gain edges: " + tempEdges.length)
            if (tempEdges.length <= edgeSelect) {
                if (sp.parameters.get[Int]('debug) >= 2)
                    println("adding all edges")
                for (index <- tempEdges) {
                    finalEdges :+= index
                    edgeSelect -= 1
                }
                if (sp.parameters.get[Int]('debug) >= 2)
                    println("upping acceptable gain")
                var nextMinGain = kernGains.max
                for (gain <- kernGains) {
                    if (gain > minGain && gain < nextMinGain)
                        nextMinGain = gain
                }
                minGain = nextMinGain
            }
            else {
                if (sp.parameters.get[Int]('debug) >= 2)
                    println("adding rand kerns")
                for (i <- 0 to edgeSelect-1) {
                    var chosenEdge = nextInt(tempEdges.length-1)
                    while (finalEdges.contains(tempEdges(chosenEdge))) {
                        chosenEdge = nextInt(tempEdges.length-1)
                    }
                    if (sp.parameters.get[Int]('debug) >= 2)
                        println(tempEdges(chosenEdge))
                    finalEdges :+= tempEdges(chosenEdge)
                    edgeSelect -= 1
                }
            }
        }
        finalEdges = finalEdges.sortWith(_ < _)
        var startKern = 0
        var endKern = finalEdges(0)
        for (i <- 1 to finalEdges.length) {
            var sps = new SPSegment(i)
            for (j <- startKern to endKern)
                sps.kernels :+= modules(j)
            sp.segments :+= sps
            startKern = endKern + 1
            if (i + 1 < finalEdges.length)
                endKern = finalEdges(i)
            else
                endKern = modules.length - 1
        }
        
        for (segment <- sp.segments) {
            for(k <- segment.kernels) {
                kernelToSPSegment += (k -> segment)
            }
            segment.initVariables()
            if (sp.parameters.get[Int]('debug) >= 2)
                println('\n' + "in:" + segment.input_rate + " out:" + segment.output_rate + " threshold:" + segment.threshold + " amp:" + segment.amplification + " run:" + segment.runtime + " state:" + segment.state + '\n')
        }
        
    }
    
    def assign_segments_to_cores() : Unit = {
        //Assign segments evenly to cores
        val segPerCore = sp.segments.length/sp.parameters.get[Int]('cores)
        val extraSegs = sp.segments.length%sp.parameters.get[Int]('cores)
        var segNum = 0
        if (sp.parameters.get[Int]('debug) >= 2) {
            println("ASSIGNING SEGS TO CORES")
            print("Min SegPerCore: " + segPerCore + "\n")
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
                    println(segNum + " ")
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