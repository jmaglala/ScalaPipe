package scalapipe.map

import scalapipe._

private[scalapipe] abstract class Mapper(val sp : ScalaPipe) 
{
    
    // Helper functions
    private def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
    private def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)
    
    // Abstract Methods
    def create_segments(): Unit
    def assign_segments_to_cores(): Unit
    
    // Concrete Methods
    private def min_buff(s: Stream): Int =
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
    private def assign_min_buffers()
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
        assign_segments_to_core()
    }
}
