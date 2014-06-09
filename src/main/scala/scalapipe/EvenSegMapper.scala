package scalapipe

import scala.math

private[scalapipe] class EvenSegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp)
{
    // kernel to segment map
    private var kernelToSegment = Map[KernelInstance,Seq[KernelInstance]]()
    private val numSegs = sp.parameters.get[Int]('schedparam)
    
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
    
    // Greedily Creates segments of size at most M
    private[this] def create_segments()
    {
        var seg = Seq[KernelInstance]()
        var size: Int = numSegs
        println(size)
        // Put everything into one segment
        var i: Int = 0
        for (k <- sp.instances) 
        {
            seg :+= k
            kernelToSegment += (k -> seg)
	    
            i += 1
            if (i > 0 && i % size == 0)
            {
                sp.segments :+= seg
                seg = Seq[KernelInstance]()
            }

        }
        if (seg.length > 0) sp.segments :+= seg
        for (segment <- sp.segments)
        {
            for(k <- segment)
            {
                kernelToSegment += (k -> segment)
            }
        }
        sp.segments.foreach(println)
    }

    // Assigns the minimum buffers to all edges
    private[this] def assign_min_buffers()
    {
        for (s <- sp.streams)
        {
            s.parameters.set('queueDepth, min_buff(s))
        }
    }
        
    // Increases cross-edge buffers to M
    private[this] def assign_cross_buffers()
    {
        // For now, we'll assum 1:1 and do everything at once
        val totalIterations = sp.parameters.get[Int]('iterations)
        val cacheSize = sp.parameters.get[Int]('cache)

        // Cross streams (connect kernels on different segments)
        val crossStreams = sp.streams.filter(s =>(kernelToSegment(s.sourceKernel) != kernelToSegment(s.destKernel) ))
        for (s <- crossStreams)
        {
            val bytes = s.sourceKernel.kernel.outputs(0).valueType.bytes
            val count = cacheSize / bytes
            println(count)
            s.parameters.set('queueDepth, count)    
        }
    }
    
    def map() 
    {
        assign_min_buffers()
        create_segments()
        assign_cross_buffers()
    }
}
