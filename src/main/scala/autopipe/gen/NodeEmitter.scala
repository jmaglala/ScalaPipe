
package autopipe.gen

import scala.collection.immutable.ListSet
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import autopipe._
import autopipe.dsl.AutoPipeObject

private[gen] abstract class NodeEmitter(
        val co: CodeObject
    ) extends Generator {

    private val checkedPorts = new ListBuffer[HashSet[String]]

    checkedPorts += new HashSet[String]

    protected def beginScope() {
        checkedPorts += new HashSet[String]
    }

    protected def endScope() {
        checkedPorts.trimEnd(1)
    }

    protected def addCheckedPort(n: String) {
        checkedPorts.last += n
    }

    protected def addCheckedPorts(l: Traversable[String]) {
        checkedPorts.last ++= l
    }

    protected def getCheckedPorts(): Seq[String] = checkedPorts.last.toList

    protected def isCheckedPort(n: String): Boolean = {
        for (s <- checkedPorts) {
            if (s.contains(n)) {
                return true;
            }
        }
        return false
    }

}

