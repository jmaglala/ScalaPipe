package scalapipe.gen

import scalapipe._
import java.io.File

private[scalapipe] abstract class ScheduleGenerator extends Generator
{
    //def getName: String
    
    // Takes a set of kernels, returns a set of segments (set of set of kernels)
    //def getSegments(): Unit // ???
    
    def emitThread(tid: Int): Unit // ???? 
    
    def emit(dir: File): Unit
}