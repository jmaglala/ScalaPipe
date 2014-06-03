package scalapipe.gen

import scalapipe._
import sclapipe.dsl._

import java.io.File

private[scalapipe] class CacheScheduleGenerator(
        val sp: ScalaPipe,
    ) extends ScheduleGenerator
{

    // The scheduler helper functions "Sched.h"
    private def emitHeader: String = {
    
    }
    
    // The code for each spawned thread
    override def emitThread(tid: Int)
    {
    
    }
    
    // Creates the Sched.h file which does the heavy lifting?
    override def emit(dir: File)
    {
        // Generate the header
        val schedHeader = new File(dir, "Sched.h")
        val schedHeaderPS = new PrintStream(new FileOutputStream(schedHeader))
        schedHeaderPS.print(emitHeader)
        schedHeaderPS.close()
    }
}