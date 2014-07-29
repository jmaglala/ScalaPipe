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
    var segmentation = Array[Array[Seq[SPSegment]]]()
    var segment_load = Array[Array[Double]]()
    
    // Calculate the load if modules i-j were all assigned to the same core
    def calc_load(i: Int, j: Int) : Double = 
    {
        val modules = sp.instances
        val cacheSize = sp.parameters.get[Int]('cache)
        var miss_time = 3
        var time = 0.0
        var space = 0
        for (mod <- modules.slice(i,j+1)) 
        {
            // Add up the modules state
            space += mod.kernelType.configs.filter(c => c.name == "state").head.value.long.toInt
            
            // Normalize the runtime
            var iterations : Double = 1
            if (mod != modules.head)
                iterations = mod.getInputs(0).gain / mod.kernelType.configs.filter(c => c.name == "inrate").head.value.long.toInt
            val normal_rt = iterations * mod.kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
            // TODO: Might need to use doubles throughout
            time += normal_rt
            
            // Add the space for the internal buffer
            if (mod.index-1 != i)
                space += mod.getInputs(0).parameters.get[Int]('queueDepth) * 4
        }
        var segments = Seq[SPSegment]()
        var load: Double = 0 
        if (space < cacheSize) 
        {
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
        else 
        {
            load = time
            segments = int_best_partition(i,j)
            for (segment <- segments) {
                if (modules(i).getInputs.length != 0)
                    load += modules(i).getInputs(0).gain * miss_time
                    
                if (modules(j).getOutputs.length != 0)
                    load += modules(j).getOutputs(0).gain * miss_time
            }
        }
        segmentation(i) :+= segments
        return load
    }

    // Find a schedule for a given maximum load on one processor
    def schedule_maxload(max_load : Int) : Array[Int] =
    {
        // Container for the return information. segments[0] holds a boolean value stating if the load was too small or not
        var segments = Array[Int]()
        val modules = sp.instances
        segments :+= 0
        var mod_start = 0
        var mod_end = 0
        var building = true
        var done = false
        for (proc_id <- 0 to sp.parameters.get[Int]('cores)-1) 
        {
            done = false
            while (building && !done)
            {
                mod_end += 1
                // If we ran out of modules
                if (mod_end + 1 >= modules.length)
                {
                    segments :+= mod_start
                    building = false
                }
                else if (segment_load(mod_start)(mod_end+1) > max_load)
                {
                    segments :+= mod_start
                    mod_start = mod_end + 1
                    mod_end = mod_end + 1
                    done = true
                }
            }
        }
        // We couldn't schedule, L was too small
        if (mod_end < modules.length-1)
            segments(0) = 1
        // We could schedule, L was probably too large
        else
            segments(0) = 0
        return segments
    }
    
    def create_segments() : Unit = {
        val modules = sp.instances
        var edges = sp.streams.toSeq.sortBy(s => (s.index))
    
        //var segid = 0
        for (i <- 0 to modules.length-1) {
            segment_load :+= Array[Double]()
            segmentation :+= Array[Seq[SPSegment]]()
            
            var segments = Seq[SPSegment]()
            for (j <- 0 to modules.length-1) {
                segment_load(i) :+= calc_load(i,j)
            }
        }
        if (sp.parameters.get[Int]('debug) >= 2)
            println("Loads calculated")
        
        var L_min = -1
        var L_max = -1
        var L_guess = 1
        
        // Find the max load
        var maxfound = false
        while (!maxfound) 
        {
            var segments = schedule_maxload(L_guess)
            if (segments(0) == 1)
                L_guess *= 2
            else
            {
                L_min = L_guess / 2
                L_max = L_guess
                maxfound = true
            }
            
        }
        if (sp.parameters.get[Int]('debug) >= 2)
            println("Max L = " + L_max)
        var L_next = L_min + (L_max - L_min)/2
        var loadfound = false
        var segments = Array[Int]()
        while (!loadfound)
        {
            L_guess = L_next
            segments = schedule_maxload(L_guess)
            if (segments(0) == 1)
                L_min = L_guess
            else
            {
                L_max = L_guess
            }
            L_next = L_min + (L_max - L_min)/2
            if (L_guess == L_next)
                loadfound = true
        }
        if (sp.parameters.get[Int]('debug) >= 2)
            println("Load found: " + L_guess)
        var segid = 0
        
        for (index <- 1 to segments.length - 1) {
            var startKern = segments(index)
            var endKern = 0
            if (index == segments.length-1) {
                endKern = modules.length - 1
            }
            else {
                endKern = segments(index+1) - 1
            }
            for (segment <- segmentation(startKern)(endKern))
            {
                segment.id = segid
                segment.tid = index-1
                segid += 1
                sp.segments :+= segment
            }
        }
        
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