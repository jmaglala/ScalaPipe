package scalapipe.map

import scala.math
import scalapipe._
import scalapipe.dsl._

private[scalapipe] abstract class Mapper(val sp : ScalaPipe) 
{
    
    // Helper functions
    def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
    def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)
    
    // Compute Stream gains
    def compute_gains()
    {
        var sorted_streams = sp.streams.toSeq.sortBy(s => (s.index))
        var gain : Double = 1
        for (stream <- sorted_streams)
        {
            if (stream.index == 1)
            {
                gain = 1.0
            }
            else
            {
                gain = gain * (stream.sourceKernel.kernel.outputs(0).rate.toDouble / stream.sourceKernel.kernel.inputs(0).rate.toDouble)
            }
            stream.gain = gain
            if (sp.parameters.get[Int]('debug) >= 2)
                println(Math.round(stream.gain))
        }
    }
    
    // Abstract Methods
    def create_segments(): Unit
    def assign_segments_to_cores(): Unit
    
    // Concrete Methods
    def min_buff(s: Stream): Int =
    {
        // We'll just use the max of the two's rates
        val sourceRate: Int = s.sourceKernel.kernel.outputs(0).rate
        val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        var newMinBufSize: Int = 1
        
        if (Math.max(sourceRate, destRate).toInt % Math.min(sourceRate, destRate).toInt == 0)
            newMinBufSize = Math.max(sourceRate, destRate).toInt
        else
            newMinBufSize = sourceRate.toInt + destRate.toInt
        return newMinBufSize        
    }

    // Assigns the minimum buffers to all edges
    def assign_min_buffers()
    {
        for (s <- sp.streams)
        {
            s.parameters.set('queueDepth, min_buff(s))
        }
    }
    
    def int_best_partition(start: Int, end: Int): Seq[SPSegment] = 
    {
        val cacheSize = sp.parameters.get[Int]('cache)

        val modules = sp.instances.slice(start,end+1)
        val mods = modules.length
        
        //Module gain from source
        var mod_rates = Seq[Int]()
        //Module state size
        var mod_size = Seq[Int]()
        //Minimum buffer sizes
        var edge_size = Seq[Int]()
        //Minimum cost of best segment up to module i
        var mod_sum = Array[Int]()
        //Best segment up to module i
        var mod_ids = Array[Array[Int]]()
        
        //Populate lists with appropriate information
        var kernel_gain = 1
        for (i <- 0 to (modules.length-1)) {
            if (i == 0) {
            kernel_gain *= modules(i).kernelType.configs.filter(c => c.name == "outrate").head.value.long.toInt
            }
            else {
            kernel_gain *= modules(i).kernelType.configs.filter(c => c.name == "outrate").head.value.long.toInt
            kernel_gain /= modules(i).kernelType.configs.filter(c => c.name == "inrate").head.value.long.toInt
            var min_buff_size = modules(i).getInputs(0).parameters.get[Int]('queueDepth) * 4
            edge_size :+= min_buff_size
            }
            var kernel_state = modules(i).kernelType.configs.filter(c => c.name == "state").head.value.long.toInt
            mod_size :+= kernel_state
            mod_rates :+= kernel_gain

            mod_sum :+= 0
            mod_ids :+= Array[Int]()
        }
        
        
        var k = 0
        var min_c = 0
        for (i <- 0 to (modules.length-1)) {
            if (i == 0) 
            {
                mod_sum(i) = mod_rates(i)
                mod_ids(i) :+= i
            }
            else 
            {
                k = i
                min_c = mod_sum(k-1) + mod_rates(k)
                for (j <- i-1 to 0 by -1) {
                    var t_size = 0
                    t_size = mod_size.slice(j,i+1).sum
                    var mod_start = modules(j)
                    var mod_end = modules(i)
                    t_size += edge_size.slice(j,i).sum
                    if (t_size <= cacheSize) {
                        var t_cost = mod_rates(i)
                        if (j-1 >= 0) {
                            t_cost = mod_sum(j-1) + mod_rates(i)
                        }
                        if (t_cost < min_c) {
                            min_c = t_cost
                            k = j
                        }
                    }
                }
                mod_sum(i) = min_c
                if (k - 1 >= 0) {
                    for (mod_id <- mod_ids(k-1)) {
                        mod_ids(i) :+= mod_id
                    }
                }
                mod_ids(i) :+= k
            }
        }
        
        val seg_starts = mod_ids(mod_ids.length-1)
        var segid = 0
        var segList = Seq[SPSegment]()
        for (index <- 0 to seg_starts.length - 1) {
            var startKern = seg_starts(index)
            var endKern = 0
            if (index == seg_starts.length-1) {
                endKern = modules.length - 1
            }
            else {
                endKern = seg_starts(index+1) - 1
            }
            var segment = Seq[KernelInstance]()
            for (kernIndex <- startKern to endKern) {
                segment :+= modules(kernIndex)
            }
            segid += 1
            var sps = new SPSegment(segid)
            sps.kernels = segment
            segList :+= sps
        }
        
        return segList
    }
    
    // Toplevel map 
    def map() 
    {   
        compute_gains()
        assign_min_buffers()
        create_segments()
        assign_segments_to_cores()
    }
}


private[scalapipe] trait AugmentBuffer extends Mapper{
    // kernel to segment map
    var kernelToSPSegment = Map[KernelInstance,SPSegment]()

    def cross_buff(s: Stream): Int = 
    {
        //val sourceRateOut: Int = s.sourceKernel.kernel.outputs(0).rate
        //val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        
        //print(sourceRate + " " + destRate + " ")
        // For now we're assuming the same size data... which is right?
        val bytes = s.sourceKernel.kernel.outputs(0).valueType.bytes
        val cacheSize: Int = sp.parameters.get[Int]('cache) / bytes
        //val t_lcm: Int = super.lcm(super.lcm(sourceRateOut,destRate),sourceRateIn)
        //val t_lcm: Int = super.lcm(sourceRateOut,destRate)
        //print("lcm: " + t_lcm + " - ")
        //if (cacheSize % t_lcm == 0) 
        //    return cacheSize
        //else
        //    return ((cacheSize / t_lcm) + 1) * t_lcm
        return cacheSize * Math.round(s.gain).toInt
    }

    // Increases cross-edge buffers to M
    def assign_cross_buffers()
    {
        // Cross streams (connect kernels on different segments)
        if (sp.parameters.get[Int]('debug) >= 2)
            println("\nASSIGNING CROSS BUFFERS")
        val crossStreams = sp.streams.filter(s =>(kernelToSPSegment(s.sourceKernel) != kernelToSPSegment(s.destKernel) ))

        for (s <- crossStreams)
        {
            val count = cross_buff(s)
            s.parameters.set('queueDepth, count)    
            s.parameters.set('crossedge, true)
        }
        if (sp.parameters.get[Int]('debug) >= 2)
            println("DONE WITH CROSS BUFFERS")
    }
    
    // Override the map function
    override def map() 
    {
        super.map()
        assign_cross_buffers()
    }
}

private[scalapipe] trait MinBufResize extends Mapper{

    var kernelToSPSegment = Map[KernelInstance,SPSegment]()

    def assign_cross_buffers()
    {
        
        // Cross streams (connect kernels on different segments)
        if (sp.parameters.get[Int]('debug) >= 2)
                println("\nASSIGNING CROSS BUFFERS")
        val crossStreams = sp.streams.filter(s =>(kernelToSPSegment(s.sourceKernel) != kernelToSPSegment(s.destKernel) ))

        for (s <- crossStreams)
        {
            var i = 0
            for (segment <- sp.segments) {
                if (s.sourceKernel == segment.kernels.last)
                    i = segment.id - 1
            }
            var newMinBufSize: Int = 1
            val outRate = Math.max(sp.segments(i).output_rate, s.sourceKernel.kernel.outputs(0).rate)
            val inRate = sp.segments(i+1).input_rate
            if (Math.max(outRate, inRate).toInt % Math.min(outRate, inRate).toInt == 0)
                newMinBufSize = Math.max(outRate, inRate).toInt
            else
                newMinBufSize = outRate.toInt + inRate.toInt
            s.parameters.set('queueDepth, newMinBufSize)
            val sourceRateOut: Int = s.sourceKernel.kernel.outputs(0).rate
            val destRate: Int   = s.destKernel.kernel.inputs(0).rate
            i += 1
        }
        if (sp.parameters.get[Int]('debug) >= 2)
            println("DONE WITH CROSS BUFFERS")
    }
    
    
    
    override def map() 
    {
        super.map()
        assign_cross_buffers()
    }
}