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
        
        var min_ids = Array[Array[Int]]()
        var min_rt = Array[Array[Int]]()
        
        //Populate lists with appropriate information
        for (i <- 0 to (modules.length-1)) {
            var kernel_rt = modules(i).kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
            mod_rt(i) = kernel_rt

            min_ids :+= Array[Int]()
            min_rt :+= Array[Int]()
        }
        
        println("mod_rt " + mod_rt)
        println("min_ids " + min_ids)
        println("min_rt " + min_rt)
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
