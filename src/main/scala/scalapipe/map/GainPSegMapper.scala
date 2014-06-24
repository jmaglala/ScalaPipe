package scalapipe.map

import scala.math
import util.Random.nextInt
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

private[scalapipe] class GainPSegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp) with AugmentBuffer
{
    def create_segments() : Unit = {
        val modules = sp.instances
        val modsLen = modules.length
        val numOfCores = sp.parameters.get[Int]('cores)
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
        
        //DEBUG
        kernGains.foreach(println)
        //println()
        //println()
        
        //Setup vars for loop control
        var edgeSelect = numOfCores
        var minGain = kernGains.min
        
        while (edgeSelect > 0) {
            var tempEdges = Array[Int]()
            ///DEBUG
            println("min: " + minGain)
            
            //Add all minimum gains to an tempEdges
            println("adding min gain edges")
            for (i <- 0 to kernGains.length-1) {
                if (kernGains(i) == minGain) {
                    tempEdges :+= i
                }
            }
            println("min gain edges: " + tempEdges.length)
            if (tempEdges.length <= edgeSelect) {
                println("adding all edges")
                for (index <- tempEdges) {
                    finalEdges :+= index
                    edgeSelect -= 1
                }
                println("upping acceptable gain")
                var nextMinGain = kernGains.max
                for (gain <- kernGains) {
                    if (gain > minGain && gain < nextMinGain)
                        nextMinGain = gain
                }
                minGain = nextMinGain
            }
            else {
                println("adding rand kerns")
                for (i <- 0 to edgeSelect-1) {
                    var chosenEdge = nextInt(tempEdges.length-1)
                    while (finalEdges.contains(tempEdges(chosenEdge))) {
                        println("conflict")
                        chosenEdge = nextInt(tempEdges.length-1)
                    }
                    //DEBUG
                    //println(tempEdges(chosenEdge))
                    finalEdges :+= tempEdges(chosenEdge)
                    edgeSelect -= 1
                }
            }
        }
        finalEdges = finalEdges.sortWith(_ < _)
        //DEBUG
        //finalEdges.foreach(println)
        var startKern = 0
        var endKern = finalEdges(0)
        println("KERN SEGMENTS - GAIN")
        println("length: " + finalEdges.length)
        for (i <- 1 to finalEdges.length) {
	    println("i: " + i)
            var sps = new SPSegment(i)
            print(startKern + " - " + endKern + " - ")
            if (i != finalEdges.length)
                println(kernGains(endKern))
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
                print(k)
                kernelToSPSegment += (k -> segment)
            }
            segment.initVariables()
            println()
        }
        
    }
    
    def assign_segments_to_cores() : Unit = {
        //Assign segments evenly to cores
        val segPerCore = sp.segments.length/sp.parameters.get[Int]('cores)
        val extraSegs = sp.segments.length%sp.parameters.get[Int]('cores)
        var segNum = 0
        println("ASSIGNING SEGS TO CORES")
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
            println()
        }
        for (seg <- sp.segments) {
            print(seg.tid)
        }
    }
}