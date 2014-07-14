package scalapipe.map


import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

// Helpers to dertmine cache size


private[scalapipe] class RuntimeSegMapper(
    val _sp: ScalaPipe
    ) extends Mapper(_sp) with AugmentBuffer
{

    // Greedily Creates segments of size at most M
    def create_segments() : Unit = 
    {
        val modules = sp.instances
        val procs = sp.parameters.get[Int]('cores)
        val mods = modules.length
        
        //Module runtime from source
        var mod_rt = Seq[Int]()
        
        var min_ids = Array[Array[Array[Int]]]()
        var min_rt = Array[Array[Int]]()
        
        //Populate lists with appropriate information
        for (i <- 0 to (modules.length-1)) {
            var kernel_rt = modules(i).kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
            mod_rt :+= kernel_rt

        }
        for (i <- 0 to (procs-1)) {
            for (j <- 0 to (mods-1)) {
                min_ids :+= Array[Array[Array[Int]]]()
                min_rt :+= Array[Array[Int]](0)
            }
        }
        
        var min_k = 0
        var t_min_rt = 0
        for (i <- 0 to (procs-1)) {
            for (j <- 0 to (mods-1)) {
                if (i == 0) {
                    min_rt(i)(j) = mod_rt.slice(0,j+1).sum
                }
                else if (j == 0) {
                    min_rt(i)(j) = 0
                }
                else {
                    min_k = j
                    t_min_rt = min_rt(i-1)(j)
                }
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
