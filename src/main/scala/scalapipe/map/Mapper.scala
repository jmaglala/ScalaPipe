package scalapipe.map

import scala.math
import scalapipe._
import scalapipe.dsl._

private[scalapipe] abstract class Mapper(val sp : ScalaPipe) 
{
    
    // Helper functions
    def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
    def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)
    
    // Abstract Methods
    def create_segments(): Unit
    def assign_segments_to_cores(): Unit
    
    // Concrete Methods
    def min_buff(s: Stream): Int =
    {
        // We'll just use the max of the two's rates
        val sourceRate: Int = s.sourceKernel.kernel.outputs(0).rate
        val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        
        if (sourceRate > destRate)
        {
            return (sourceRate)
        }
        else
        {
            return (destRate)
        }
    }

    // Assigns the minimum buffers to all edges
    def assign_min_buffers()
    {
        for (s <- sp.streams)
        {
            s.parameters.set('queueDepth, min_buff(s))
        }
    }
    
    // Toplevel map 
    def map() 
    {
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
        //var sourceRateIn: Int = 1
        //if (s.sourceKernel.kernel.inputs.length != 0)
        //    sourceRateIn = s.sourceKernel.kernel.inputs(0).rate
        val sourceRateOut: Int = s.sourceKernel.kernel.outputs(0).rate
        val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        
        //print(sourceRate + " " + destRate + " ")
        // For now we're assuming the same size data... which is right?
        val bytes = s.sourceKernel.kernel.outputs(0).valueType.bytes
        val cacheSize: Int = sp.parameters.get[Int]('cache) / bytes
        //val t_lcm: Int = super.lcm(super.lcm(sourceRateOut,destRate),sourceRateIn)
        val t_lcm: Int = super.lcm(sourceRateOut,destRate)
        print("lcm: " + t_lcm + " - ")
        if (cacheSize % t_lcm == 0) {
            println(cacheSize)
            return cacheSize
        }
        else {
            println(((cacheSize / t_lcm) + 1) * t_lcm)
            return ((cacheSize / t_lcm) + 1) * t_lcm
        }
    }

    // Increases cross-edge buffers to M
    def assign_cross_buffers()
    {
        // Cross streams (connect kernels on different segments)
        println()
        println("ASSIGNING CROSS BUFFERS")
        val crossStreams = sp.streams.filter(s =>(kernelToSPSegment(s.sourceKernel) != kernelToSPSegment(s.destKernel) ))

        for (s <- crossStreams)
        {
            val count = cross_buff(s)
            s.parameters.set('queueDepth, count)    
        }
        println("DONE WITH CROSS BUFFERS")
    }

    def set_rates()
    {
        for (s <- sp.segments)
        {
            val sourceRate = s.input_rate
            val destRate = s.output_rate
            println(sourceRate + " " + destRate)
            var inQueueSize = 1
            if (s != sp.segments.head)
                inQueueSize = s.kernels.head.getInputs(0).parameters.get[Int]('queueDepth)
            var outQueueSize = 1
            if (s != sp.segments.last)
                outQueueSize = s.kernels.last.getOutputs(0).parameters.get[Int]('queueDepth)
            if (outQueueSize % sourceRate != 0 && sourceRate != -1 || inQueueSize % destRate != 0 && destRate != -1) {
                val ratio: Double = 1 - (Math.min(sourceRate,destRate)/ Math.max(sourceRate,destRate))
                println(ratio)
                if (ratio < sp.parameters.get[Double]('bufPercent)) {
                    //sp.parameters.set('bufPercent, ratio)
                    //sp.parameters.set('minSegFires, ratio)
                    println("setting to " + ratio)
                }
            }
        }
        //sp.parameters.set('bufPercent, sp.parameters.get[Double]('bufPercent)-.01)
    }
    
    // Override the map function
    override def map() 
    {
        super.map()
        assign_cross_buffers()
        set_rates()
    }
}