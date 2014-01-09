
package autopipe.dsl

import language.implicitConversions
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import autopipe._

class AutoPipeBlock(val name: String) extends EmbeddedControls {

    private val labelCounter = new LabelMaker("S")
    private[autopipe] val inputs = new ListBuffer[BlockInput]
    private[autopipe] val outputs = new ListBuffer[BlockOutput]
    private[autopipe] val states = new ListBuffer[BlockLocal]
    private[autopipe] val configs = new ListBuffer[BlockConfig]
    private[autopipe] val externals = new ListBuffer[Platforms.Value]
    private[autopipe] val dependencies = new DependencySet
    private[autopipe] val scopeStack = new ListBuffer[autopipe.Scope]
    private[autopipe] var loopBack = false

    def this() = this(LabelMaker.getBlockLabel)

    implicit val im = this

    scopeStack += new autopipe.Scope(this, NodeType.BLOCK)

    private[autopipe] def getRoot: ASTNode = {
        if (scopeStack.size > 1) {
            Error.raise("missing end", this)
        }
        scopeStack.last.handleEnd
    }

    private[autopipe] def isInternal(p: Platforms.Value): Boolean = {
        !externals.contains(p)
    }

    private[autopipe] def setLoopBack {
        super.__assign(loopBack, true)
    }

    private def getLabel: String = labelCounter.next()

    def inputType(i: Int): AutoPipeType = inputs(i).t

    def outputType(i: Int): AutoPipeType = outputs(i).t

    def output(t: AutoPipeType, n: Symbol = null): AutoPipeVariable = {
        val label = if (n != null) n.name else getLabel
        val node = new AutoPipeVariable(label, this)
        outputs += new BlockOutput(label, t)
        node
    }

    def input(t: AutoPipeType, n: Symbol = null): AutoPipeVariable = {
        val label = if (n != null) n.name else getLabel
        val node = new AutoPipeVariable(label, this)
        inputs += new BlockInput(label, t)
        node
    }

    def config(t: AutoPipeType, n: Symbol, v: Any = null): AutoPipeVariable = {
        val node = new AutoPipeVariable(n.name, this)
        configs += new BlockConfig(n.name, t, v)
        node
    }

    def external(platform: String, file: String = name) {
        if (!Platforms.values.exists( p => p.toString == platform)) {
            Error.raise("invalid platform: " + platform)
        }
        externals += Platforms.withName(platform)
    }

    def local(t: AutoPipeType, v: Any = null): AutoPipeVariable = {
        val label = getLabel
        states += new BlockLocal(label, t.create(), v)
        new AutoPipeVariable(label, this)
    }

    def cast(expr: ASTNode, t: AutoPipeType): ASTNode = {
        ASTConvertNode(expr, t.create(), this)
    }

    def __ifThenElse[T](cond: Boolean, thenp: => T, elsep: => T): T = {
        super.__ifThenElse(cond, thenp, elsep)
    }

    def __ifThenElse[A <% ASTNode, T](cond: A, thenp: => T,
                                                 elsep: => T): ASTNode = {
        scopeStack += new autopipe.Scope(this, NodeType.IF, cond)
        thenp
        scopeStack.last.handleElse
        elsep
        val prev = scopeStack.last
        scopeStack.trimEnd(1)
        scopeStack.last += prev.handleEnd
        null      // FIXME: support 'if' statements as part of an expression.
    }

    def __whileDo[A <% ASTNode, T](cond: A, body: => T) {
        scopeStack += new autopipe.Scope(this, NodeType.WHILE, cond)
        body
        val prev = scopeStack.last
        scopeStack.trimEnd(1)
        scopeStack.last += prev.handleEnd
    }

    def __equal[A <% ASTNode, B <% ASTNode](a: A, b: B): ASTNode = {
        ASTOpNode(NodeType.eq, a, b, this)
    }

    def __assign[A <% ASTNode, B <% ASTNode](a: A, b: B): ASTNode = {
        ASTAssignNode(a, b, this)
    }

    def switch[A <% ASTNode, T](cond: A)(cases: => T) {
        scopeStack += new autopipe.Scope(this, NodeType.SWITCH, cond)
        cases
        val prev = scopeStack.last
        scopeStack.trimEnd(1)
        scopeStack.last += prev.handleEnd
    }

    def when[A <% ASTNode, B](cond: A)(body: => B) {
        body
        scopeStack.last.handleWhen(cond)
    }

    def others[T](body: => T) {
        body
        scopeStack.last.handleWhen(null)
    }

    def If[T <% ASTNode](cond: T) {
        scopeStack += new autopipe.Scope(this, NodeType.IF, cond)
    }

    def While[T <% ASTNode](cond: T) {
        scopeStack += new autopipe.Scope(this, NodeType.WHILE, cond)
    }

    def Else {
        scopeStack.last.handleElse
    }

    def ElseIf[T <% ASTNode](cond: T) {
        scopeStack.last.handleElseIf(cond)
    }

    def End {
        val prev = scopeStack.last
        scopeStack.trimEnd(1)
        scopeStack.last += prev.handleEnd
    }

    def stop {
        ASTStopNode(this)
    }

    def abs[T <% ASTNode](s: T) = s.abs

    def exp[T <% ASTNode](s: T) = s.exp

    def log[T <% ASTNode](s: T) = s.log

    def sqrt[T <% ASTNode](s: T) = s.sqrt

    def avail(s: ASTSymbolNode) = ASTAvailableNode(s.symbol, this)

    def addr(n: ASTNode) = ASTOpNode(NodeType.addr, n, null, this)

    def sizeof(t: AutoPipeType) = new IntLiteral(t.create.bits / 8, this)

    def sizeof(n: ASTNode) = ASTOpNode(NodeType.sizeof, n, null, this)

    implicit def bool(b: Boolean) = new IntLiteral(b, this)

    implicit def byte(b: Byte) = new IntLiteral(b, this)

    implicit def char(c: Char) = new IntLiteral(c, this)

    implicit def short(s: Short) = new IntLiteral(s, this)

    implicit def int(i: Int) = new IntLiteral(i, this)

    implicit def long(l: Long) = new IntLiteral(l, this)

    implicit def float(f: Float) = new FloatLiteral(f, this)

    implicit def double(d: Double) = new FloatLiteral(d, this)

    implicit def func(f: AutoPipeFunction) = {
        dependencies.add(f.dependencies)
        ASTCallNode(f, this)
    }

    implicit def string(s: String) = new StringLiteral(s, this)

    implicit def builder(b: AutoPipeVariable) = b.create

}

