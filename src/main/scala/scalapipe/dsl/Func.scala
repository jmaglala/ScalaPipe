package scalapipe.dsl

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scalapipe._

class Func(_name: String) extends Kernel(_name) {

    def this() = this(LabelMaker.getKernelLabel)

    // Include paths needed for this function.
    def ipath(n: String) {
        dependencies.add(DependencySet.IPath, n)
    }

    // Library paths needed for this function.
    def lpath(n: String) {
        dependencies.add(DependencySet.LPath, n)
    }

    // Libraries needed for this function.
    def library(n: String) {
        dependencies.add(DependencySet.Library, n)
    }

    // Include files needed for this function.
    def include(n: String) {
        dependencies.add(DependencySet.Include, n)
    }

    // Arguments to this function.
    def argument(t: Type) {
        input(t)
    }

    // Return type.
    def returns(t: Type) {
        output(t)
    }

    // Return a value.
    def __return[T <% ASTNode](result: T) = {
        if (outputs.isEmpty) {
            outputs += new KernelOutput(getLabel, ValueType.any, 1)
        }
        ASTReturnNode(result, this)
    }

}
