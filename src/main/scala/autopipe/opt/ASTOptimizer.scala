
package autopipe.opt

import scala.collection.immutable.ListSet
import scala.math.max
import autopipe._

private[autopipe] case class ASTOptimizer(val co: CodeObject) {

    def optimize(root: ASTNode): ASTNode = {
        removeDupBlocks(root)
    }

    private def removeDupBlocks(root: ASTNode): ASTNode = root match {

        case in: ASTIfNode            =>
            val newTrue = removeDupBlocks(in.iTrue)
            val newFalse = removeDupBlocks(in.iFalse)
            ASTIfNode(in.cond, newTrue, newFalse)
        case wn: ASTWhileNode        =>
            ASTWhileNode(wn.cond, removeDupBlocks(wn.body))
        case bn: ASTBlockNode        => {
                val subnodes = bn.children.map(removeDupBlocks)
                if (subnodes.size == 1) {
                    subnodes.head
                } else {
                    ASTBlockNode(subnodes)
                }
            }
        case other => other

    }

}

