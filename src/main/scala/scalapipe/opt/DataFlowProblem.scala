package scalapipe.opt

import scalapipe._

abstract class DataFlowProblem {

    type T
    type Result = Map[Int, Set[T]]

    def forward: Boolean

    def init(kt: KernelType, graph: IRGraph): Set[T]

    def gen(sb: StateBlock, in: Set[T]): Set[T]

    def kill(sb: StateBlock, in: Set[T]): Set[T]

    def meet(a: Set[T], b: Set[T]): Set[T]

    final def transfer(sb: StateBlock, in: Set[T]): Set[T] = {
        val k = kill(sb, in)
        val g = gen(sb, in)
        (in -- k) ++ g
    }

    protected def isPort(s: BaseSymbol): Boolean = s.isInstanceOf[PortSymbol]

    protected def isVariable(s: BaseSymbol): Boolean = s match {
        case t: TempSymbol  => true
        case s: StateSymbol => true
        case _              => false
    }

    private def inputs(sb: StateBlock, graph: IRGraph): Seq[StateBlock] = {
        if (forward) {
            graph.inLinks(sb)
        } else {
            graph.links(sb)
        }
    }

    private def outputs(sb: StateBlock, graph: IRGraph): Seq[StateBlock] = {
        if (forward) {
            graph.links(sb)
        } else {
            graph.inLinks(sb)
        }
    }

    final def solve(kt: KernelType, graph: IRGraph): Result = {

        var before = Map[Int, Set[T]]()
        var after = Map[Int, Set[T]]()
        var work = Set[StateBlock]()
        val top = init(kt, graph)

        graph.blocks.foreach { block =>
            before += (block.label -> top)
            after += (block.label -> top)
            work += block
        }

        while (!work.isEmpty) {
            val block = work.head
            work = work.filterNot(_ == block)
            val in = inputs(block, graph)
            val temp = if (!in.isEmpty) {
                in.tail.foldLeft(after(in.head.label)) { (a, b) =>
                    meet(a, after(b.label))
                }
            } else {
                before(block.label)
            }
            val tempSet = temp.toSet
            val updated = transfer(block, tempSet)
            if (after(block.label) != updated) {
                before += (block.label -> tempSet)
                after += (block.label -> updated)
                work ++= outputs(block, graph)
            }
        }

        if (forward) before else after

    }

}
