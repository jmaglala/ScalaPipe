package scalapipe.gen

import scalapipe._
import java.io.File

private[scalapipe] abstract class ScheduleGenerator extends Generator
{
    def getName: String
    
    def emit(dif: File): Unit
}