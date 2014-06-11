package scalapipe.dsl

import scalapipe.{KernelInstance}

class SPSegment (_id: Int) {

    private[scalapipe] var kernels = Seq[KernelInstance]()

    private[scalapipe] val id = _id
    private[scalapipe] val tid = 0
    
}