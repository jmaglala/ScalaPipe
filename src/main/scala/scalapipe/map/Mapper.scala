package scalapipe.map

import scalapipe._

private[scalapipe] abstract class Mapper(val sp : ScalaPipe) 
{
    def map(): Unit
}