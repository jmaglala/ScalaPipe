package scalapipe

private[scalapipe] class SegCacheMapper(
    val _sp: ScalaPipe
) extends Mapper(_sp)
{
    def map() 
    {
        var seg = Seq[KernelInstance]()
        // Put everything into one segment
        for (k <- sp.instances) 
        {
            seg :+= k
        }
        sp.segments :+= seg
    }
}