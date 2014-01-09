
package blocks

import autopipe.dsl._

class DuplicateBlock(t: AutoPipeType, n: Int = 2) extends AutoPipeBlock {

    val x0 = input(t)
    val temp = local(t)

    temp = x0
    for (i <- 0 until n) {
        val o = output(t)
        o = temp
    }

}

