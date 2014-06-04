package scalapipe

import scala.math

private[scalapipe] class SegCacheMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp)
{
    // kernel to segment map
    private var kernelToSegment = Map[KernelInstance,Seq[KernelInstance]]()

    private[this] def min_buff(s: Stream): Int =
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
    
    private[this] def create_segments()
    {
        var seg = Seq[KernelInstance]()
        var mid: Int = seg.length / 2
        // Put everything into one segment
        var i: Int = 0
        for (k <- sp.instances) 
        {
            if (i == mid)
            {
                sp.segments :+= seg
                seg = Seq[KernelInstance]()
            }
            
            seg :+= k
            kernelToSegment += (k -> seg)
        }
        sp.segments :+= seg
        
        sp.segments.foreach(println)
    }
    
    private[this] def assign_buffers()
    {
        // Internal steams (connect kernels on the same segment)
        val internalStreams = sp.streams.filter(s =>(kernelToSegment(s.sourceKernel) == kernelToSegment(s.destKernel) ))
        for (s <- internalStreams)
        {
            // Set the internal edges to minimum buffers
            val minbuff: Int = min_buff(s)
            s.parameters.set('queueDepth, minbuff)
        }
        
        // For now, we'll assum 1:1 and do everything at once
        val totalIterations = sp.parameters.get[Int]('iterations)

        // Cross streams (connect kernels on different segments)
        val crossStreams = sp.streams.filter(s =>(kernelToSegment(s.sourceKernel) != kernelToSegment(s.destKernel) ))
        for (s <- crossStreams)
        {
            s.parameters.set('queueDepth, totalIterations)    
        }
    }
    
    def map() 
    {
        create_segments()
        assign_buffers()
    }
}