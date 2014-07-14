package scalapipe.map

import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

private[scalapipe] class RuntimeSegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp) with MinBufResize
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
            mod_rt :+= kernel_rt

            min_ids :+= Array[Int]()
            min_rt :+= Array[Int]()
        }
        
        println("mod_rt " + mod_rt)
        println("min_ids " + min_ids)
        println("min_rt " + min_rt)

    }
    
    def assign_segments_to_cores() : Unit = {
        //Since there are as many segments as cores, assign segment i to core i
        for (i <- 0 to sp.segments.length-1)
            sp.segments(i).tid = i
    }
}
