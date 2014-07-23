package scalapipe.map


import scala.math
import scalapipe._
import scalapipe.dsl._
import scalapipe.dsl.SPSegment

// Helpers to dertmine cache size


private[scalapipe] class BothSegMapper(
    val _sp: ScalaPipe
    ) extends Mapper(_sp) with AugmentBuffer
{
    def create_segments() : Unit = {
        val modules = sp.instances
        var edges = sp.streams.toSeq.sortBy(s => (s.index))
    
        var segment_load = Array[Seq[Double]]()
        var segmentation = Array[Seq[SPSegment]]()
        
        //var segid = 0
        for (i <- 0 to modules.length-1) {
            segment_load :+= Seq[Double]()
            segmentation :+= Seq[SPSegment]()
            
            var segments = Seq[SPSegment]()
            for (j <- 0 to modules.length-1) {
                //CALC LOAD
                var miss_time = 3
                var time = 0
                var space = 0
                var load: Double = 0
                if (i <= j) {
                    for (mod <- modules.slice(i,j+1)) {
                        val modState = mod.kernelType.configs.filter(c => c.name == "state").head.value.long.toInt
                        val modRT = mod.kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
                        var iterations = 0
                        if (mod != modules.head)
                            iterations = mod.getInputs(0).parameters.get[Int]('queueDepth) / mod.kernel.inputs(0).rate
                        else 
                            iterations = mod.getOutputs(0).parameters.get[Int]('queueDepth) / mod.kernel.outputs(0).rate
                        
                        space += modState
                        //MAYBE WRONG i-1? index+1?
                        if (mod.index-1 != i) {
                            space += mod.getInputs(0).parameters.get[Int]('queueDepth) * 4
                        }
                        //println("iters: " + iterations + " modRT: " + modRT)
                        time += iterations * modRT
                    }
                    val cacheSize = sp.parameters.get[Int]('cache)
                    //println(i + " to " + j)
                    //println("space: " + space + " cache: " + cacheSize + " time: " + time)
                    if (space < cacheSize) {
                        var miss_rate: Double = 0
                        if (modules(i).getInputs.length != 0)
                                miss_rate += modules(i).getInputs(0).gain
                        if (modules(j).getOutputs.length != 0)
                                miss_rate += modules(j).getOutputs(0).gain
                        load = time + miss_time * miss_rate
                        val seg = new SPSegment(0)
                        for (mod_id <- i to j) {
                            seg.kernels :+= modules(mod_id)
                        }
                        segments :+= seg
                    }
                    else {
                        load = time
                        segments = int_best_partition(i,j)
                        for (segment <- segments) {
                            if (modules(i).getInputs.length != 0)
                                load += modules(i).getInputs(0).gain * miss_time
                                
                            if (modules(j).getOutputs.length != 0)
                                load += modules(j).getOutputs(0).gain * miss_time
                        }
                    }
                }
                //println("load: " + load)
                //println()
                //END CALC LOAD
                segment_load(i) :+= load
                segmentation(i) = segments
            }
        }
        println("Loads calculated")
        
        var L_min = -1
        var L_max = -1
        var L_guess = 1
        var endLoop = false
        while (endLoop == false) {
            //START SCHEDULE_MAXLOAD
            var segments = Array[Int]()
            var mod_start = 0
            var mod_end = 0
            var building = true
            for (proc_id <- 0 to sp.parameters.get[Int]('cores)-1) {
                while (building) {
                    mod_end += 1
                    if (mod_end + 1 >= modules.length) {
                        segments :+= mod_start
                        building = false
                    }
                    else if (segment_load(mod_start)(mod_end+1) > L_guess) {
                        segments :+= mod_start
                        mod_start = mod_end+1
                        mod_end = mod_end+1
                        building = false
                    }
                }
                if (mod_end+1 <= modules.length) {
                    building = true
                }
            }
            
            if (mod_end < modules.length-1) {
                L_guess *= 2
            }
            else {
                L_min = L_guess / 2
                L_max = L_guess
                endLoop = true
            }
        }
        println("Max L = " + L_max)
        
        var L_next = L_min + (L_max - L_min)/2
        endLoop = false
        var segment_starts = Array[Int]()
        while (endLoop == false) {
            L_guess = L_next
            println("Guessing L =  " + L_guess)
            var segments = Array[Int]()
            var mod_start = 0
            var mod_end = 0
            var building = true
            for (proc_id <- 0 to sp.parameters.get[Int]('cores)-1) {
                while (building) {
                    mod_end += 1
                    if (mod_end + 1 >= modules.length) {
                        segments :+= mod_start
                        building = false
                    }
                    if (segment_load(mod_start)(mod_end) > L_guess) {
                        segments :+= mod_start
                        mod_start = mod_end+1
                        mod_end = mod_end+1
                        building = false
                    }
                }
                if (mod_end+1 < modules.length) {
                    building = true
                }
            }
            //SMALL = TRUE
            if (mod_end < modules.length-1) {
                L_min = L_guess
            }
            else {
                L_max = L_guess
            }
            L_next = L_min + (L_max - L_min)/2
            //SMALL = FALSE
            if (L_next == L_guess) {
                segment_starts = segments
                endLoop = true
            }
        }
        var segid = 0
        var segList = Seq[SPSegment]()
        for (index <- 0 to segment_starts.length - 1) {
            var startKern = segment_starts(index)
            var endKern = 0
            if (index == segment_starts.length-1) {
                endKern = modules.length - 1
            }
            else {
                endKern = segment_starts(index+1) - 1
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
        for (i <- 0 to sp.segments.length-1)
            sp.segments(i).tid = i
    }
}