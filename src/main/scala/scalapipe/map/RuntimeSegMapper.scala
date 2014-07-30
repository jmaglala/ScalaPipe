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
        var mod_rt = Array[Double]()
        
        var min_ids = Array[Array[Array[Int]]]()
        var min_rt = Array[Array[Double]]()
        
        //Populate lists with appropriate information
        for (mod <- modules) {
            var kernel_rt = mod.kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
            var iterations : Double = 1
            if (mod != modules.head)
                iterations = mod.getInputs(0).parameters.get[Int]('queueDepth)/ mod.kernelType.configs.filter(c => c.name == "inrate").head.value.long.toInt
            var normal_rt = kernel_rt * iterations
            mod_rt :+= normal_rt
            println(mod + " " + kernel_rt + " " +normal_rt)
        }
        println(mod_rt.sum)
        for (i <- 0 to (procs-1)) {
            var tempArray1 = Array[Array[Int]]()
            var tempArray2 = Array[Double]()
            for (j <- 0 to (mods-1)) {
                tempArray1 :+= Array[Int]()
                tempArray2 :+= 0.0
            }
            min_ids :+= tempArray1
            min_rt :+= tempArray2
        }
        
        var min_k = 0
        var t_min_rt = 0.0
        var seg_rt = 0.0
        for (i <- 0 to (procs-1)) {
            for (j <- 0 to (mods-1)) {
                if (i == 0) {
                    min_rt(i)(j) = mod_rt.slice(0,j+1).sum
                }
                else if (j == 0) {
                    min_rt(i)(j) = 0.0
                }
                else {
                    min_k = j
                    t_min_rt = min_rt(i-1)(j)
                    for (k <- (j-1) to 0 by -1) {
                        seg_rt = mod_rt.slice(k+1,j+1).sum
                        if (Math.max(seg_rt,min_rt(i-1)(k)) < t_min_rt) {
                            min_k = k
                            t_min_rt = Math.max(seg_rt, min_rt(i-1)(k))
                        }
                    }
                    min_rt(i)(j) = t_min_rt
                    min_ids(i)(j) :+= min_k
                    min_ids(i-1)(min_k).foreach(min_ids(i)(j) :+= _)
                }
            }
        }
        var segs = Array[Int]()
        segs :+= 0
        min_ids(procs-1)(mods-1).foreach(segs :+= _)
        
        segs = segs.sortWith(_ < _)
        
        segs.foreach(println)
        
        val seg_starts = segs(segs.length-1)
        var segid = 0
        var segList = Seq[SPSegment]()
        for (index <- 0 to segs.length - 1) {
            var startKern = segs(index)
            var endKern = 0
            if (index == segs.length-1) {
                endKern = modules.length - 1
            }
            else {
                endKern = segs(index+1) - 1
            }
            var segment = Seq[KernelInstance]()
            println("start: " + startKern + " end: " + endKern)
            for (kernIndex <- startKern to endKern) {
                segment :+= modules(kernIndex)
            }
            segid += 1
            var sps = new SPSegment(segid)
            sps.kernels = segment
            segList :+= sps
        }
        sp.segments = segList
        
        for (segment <- sp.segments) {
            print(segment.id + ") ")
            for(k <- segment.kernels) {
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
        //Since there are as many segments as cores, assign segment i to core i
        for (i <- 0 to sp.segments.length-1)
            sp.segments(i).tid = i
    }
}
