package scalapipe.map

import scalapipe._
import scalapipe.dsl._


// A class that will create segments and augment buffer sizes to M
private[scalapipe] abstract class SegMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp)
{

    // kernel to segment map
    var kernelToSPSegment = Map[KernelInstance,SPSegment]()

    def cross_buff(s: Stream): Int = 
    {
        val sourceRate: Int = s.sourceKernel.kernel.outputs(0).rate
        val destRate: Int   = s.destKernel.kernel.inputs(0).rate
        
        // For now we're assuming the same size data... which is right?
        val bytes = s.sourceKernel.kernel.outputs(0).valueType.bytes
        val cacheSize = sp.parameters.get[Int]('cache) / bytes
        val t_lcm = super.lcm(sourceRate,destRate) 
        if (cacheSize % t_lcm == 0) return cacheSize
        else return ((cacheSize / t_lcm) + 1) * t_lcm
    }

    // Increases cross-edge buffers to M
    def assign_cross_buffers()
    {
        // Cross streams (connect kernels on different segments)
        val crossStreams = sp.streams.filter(s =>(kernelToSPSegment(s.sourceKernel) != kernelToSPSegment(s.destKernel) ))

        for (s <- crossStreams)
        {
            val count = cross_buff(s)
            s.parameters.set('queueDepth, count)    
        }
    }

    
    // Override the map function
    override def map() 
    {
        super.map()
        assign_cross_buffers()
    }
}