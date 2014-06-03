package scalapipe

private[scalapipe] abstract class Mapper(val sp : Scalapipe) 
{
    def map(): Unit
}