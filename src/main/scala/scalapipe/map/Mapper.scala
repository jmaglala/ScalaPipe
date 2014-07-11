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
        val sourceRateOut: Int = s.sourceKernel.kernel.outputs(0).rate
        val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        
        //print(sourceRate + " " + destRate + " ")
        // For now we're assuming the same size data... which is right?
        val bytes = s.sourceKernel.kernel.outputs(0).valueType.bytes
        val cacheSize: Int = sp.parameters.get[Int]('cache) / bytes
        //val t_lcm: Int = super.lcm(super.lcm(sourceRateOut,destRate),sourceRateIn)
        val t_lcm: Int = super.lcm(sourceRateOut,destRate)
        //print("lcm: " + t_lcm + " - ")
        if (cacheSize % t_lcm == 0) 
            return cacheSize
        else
            return ((cacheSize / t_lcm) + 1) * t_lcm
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