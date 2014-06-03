package scalapipe

private[scalapipe] abstract class Mapper(val sp : ScalaPipe) 
{
    def map(): Unit
}