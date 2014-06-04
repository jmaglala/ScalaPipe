package scalapipe.gen

import scalapipe._
import scalapipe.dsl._

import java.io.File

private[scalapipe] class CacheScheduleGenerator(
        val sp: ScalaPipe
    ) extends ScheduleGenerator with CGenerator
{

    // The scheduler helper functions "Sched.h"
    //private def emitHeader: String = {
    
    //}
    
    // The code for each spawned thread
    override def emitThread(tid: Int)
    {
      val name = sp.instances.head.name
      val instance = sp.instances.head.label
      val id = 0;
      write(s"static void *run_thread$id(void *arg)")
	enter
	write(s"{")
	write(s"if(setjmp($instance.env) == 0) {")
	  enter
	  write(s"sp_${name}_run(&$instance.priv);")
	  leave
	  write(s"return NULL")
	  write(s"}")
	leave
      write(s"}")
    }
    
    // Creates the Sched.h file which does the heavy lifting?
    override def emit(dir: File)
    {
        /* Generate the header
        val schedHeader = new File(dir, "Sched.h")
        val schedHeaderPS = new PrintStream(new FileOutputStream(schedHeader))
        schedHeaderPS.print(emitHeader)
        schedHeaderPS.close()*/
    }
}