package scalapipe.map

import scala.math
import util.Random.nextInt
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

private[scalapipe] class RandomSegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp)
{
    def create_segments() : Unit = {
        val modules = sp.instances
        val modsLen = modules.length
        val numOfCores = sp.parameters.get[Int]('cores)
        var rand = Array[Int]()
        
        //If there are less modules than cores, give each module its own segment and core
        if (modsLen < numOfCores) {
            for (i <- 1 to modsLen) {
                var sps = new SPSegment(i)
                sps.kernels :+= modules(i-1)
                print(i + " ")
                sps.kernels.foreach(println)
                sp.segments :+= sps
            }
            return
        }
        
        //Choose random edges to segment kernels
        for (i <- 0 to (numOfCores-1)) {
            rand :+= nextInt(modsLen)
            //Make sure the same edge isn't chosen twice
            while (rand.slice(0,i).contains(rand.last))
                rand(i) = nextInt(modsLen)
        }
        //Sort edges from smallest to largest
        rand = rand.sortWith(_ < _)
        rand.slice(0,rand.length).foreach(print)
        println()
        
        //Create segments with the chosen random edges
        var segid = 0
        for (i <- 0 to rand.length-1) {
            var segment = Seq[KernelInstance]()
            var startKern = 0
            var endKern = 0
            if (i != rand.length-1)
                endKern = rand(i+1)-1
            else
                endKern = modsLen-1
            if (i != 0)
                startKern = rand(i)
            for (kernIndex <- startKern to endKern)
                segment :+= modules(kernIndex)

            segment.foreach(print)
            println()
            
            segid += 1
            var sps = new SPSegment(segid)
            sps.kernels = segment
            sp.segments :+= sps
        }
    }
    def assign_segments_to_cores() : Unit = {
        //Since there are as many segments as cores, assign segment i to core i
        for (i <- 0 to sp.segments.length-1)
            sp.segments(i).tid = i
    }
}