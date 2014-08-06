package scalapipe.map

import scala.math
import util.Random.nextInt
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
        
        var min_ids = Array[Array[Array[Int]]]()
        var min_rt = Array[Array[Int]]()
        
        //Populate lists with appropriate information
        for (mod <- modules) {
            var kernel_rt = mod.kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
            var iterations : Double = 1
            if (mod != modules.head)
                iterations = mod.getInputs(0).gain / mod.kernelType.configs.filter(c => c.name == "inrate").head.value.long.toInt * sp.instances(0).kernel.outputs(0).rate
            var normal_rt = kernel_rt * iterations
            mod_rt :+= normal_rt.toInt

        }
        for (i <- 0 to (procs-1)) {
            var tempArray1 = Array[Array[Int]]()
            var tempArray2 = Array[Int]()
            for (j <- 0 to (mods-1)) {
                tempArray1 :+= Array[Int]()
                tempArray2 :+= 0
            }
            min_ids :+= tempArray1
            min_rt :+= tempArray2
        }
        
        var segs = Array[Int]()
        segs :+= 0
        
        //Calculate total normalized runtime
        var total_rt = 0
        mod_rt.foreach(total_rt += _)
        
        var largeKerns = Array[Int]()
        for (i <- 0 to mod_rt.length-1) {
            if (mod_rt(i) > total_rt/procs) {
                if (sp.parameters.get[Int]('debug) >= 2)
                    println(i + " is too big")
                largeKerns :+= i
            }
        }
        
        val largeKernMinSegs = 2 * largeKerns.length + 1
        if (largeKerns.length != 0) {
            for (segIndex <- largeKerns) {
                if (segs.contains(segIndex) == false) {
                    segs :+= segIndex
                    println("adding " + segIndex)
                }
                if (segIndex+1 <= mods-1 && segs.contains(segIndex+1) == false) {
                    segs :+= segIndex+1
                    println("adding next " + (segIndex+1))
                }
            }
            if (procs - largeKernMinSegs > 0) {
                println("less than 0")
                while (segs.length < procs && segs.length < mods) {
                    println("stuck here")       
                    var rand = nextInt(mods)
                    //Make sure the same edge isn't chosen twice
                    while (segs.contains(rand))
                        rand = nextInt(mods)
                    segs :+= rand
                }
            }
        }
        else {
            var min_k = 0
            var t_min_rt = 0
            var seg_rt = 0
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
            min_ids(procs-1)(mods-1).foreach(segs :+= _)
        }
        
        segs = segs.sortWith(_ < _)
        
        if (sp.parameters.get[Int]('debug) >= 2)
            println("PRINTING SEG STARTS")
        //segs.foreach(println)
        //println()
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
            if (sp.parameters.get[Int]('debug) >= 2)
                println("start: " + startKern + " end: " + endKern)
            segid += 1
            var sps = new SPSegment(segid)
            for (kernIndex <- startKern to endKern) {
                sps.kernels :+= modules(kernIndex)
                sps.runtime += mod_rt(kernIndex)
            }
            segList :+= sps
        }
        sp.segments = segList
        
        for (segment <- sp.segments) {
            if (sp.parameters.get[Int]('debug) >= 2)
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
        if (sp.parameters.get[Int]('debug) >= 2) {
            println("ASSIGNING SEGS TO CORES")
        }
        val cores = sp.parameters.get[Int]('cores)
        //If it found P segments, assign one segment per core
        if (sp.segments.length == cores) {
            for (i <- 0 to sp.segments.length-1)
                sp.segments(i).tid = i
        }
        //Otherwise, binpack emptiest first
        else {
            //Slice out all segments 
            var segments = sp.segments.slice(1,sp.segments.length).sortWith(_.runtime > _.runtime)
            
            //Create array to keep track of assigned runtime per core
            var procTotalRT = Array[Double]()
            
            //Assign the first segment to first core and update the total runtime array
            sp.segments(0).tid = 0
            procTotalRT :+= sp.segments(0).runtime
            
            //Append empty ints to the runtime array to be used in the next loop
            for (i <- 1 to cores-1)
                procTotalRT :+= 0.0   
            //For the unassigned segments, find the minimum RT of all threads, assign the segment to that thread, and update the runtime array
            for (i <- 0 to segments.length-1) {
                var targetProc = procTotalRT.indexOf(procTotalRT.min)
                segments(i).tid = targetProc
                procTotalRT(targetProc) += segments(i).runtime
            }
        }
        for (i <- 0 to cores - 1) {
            print("Core " + (i+1) + ": ")
            for (segment <- sp.segments) {
                if (segment.tid == i)
                    print(" " + segment.id)
            }
            println()
        }
        
        if (sp.parameters.get[Int]('debug) >= 2)
            println("DONE ASSIGNING SEGS TO CORES")
    }
}
