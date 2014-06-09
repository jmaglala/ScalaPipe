package scalapipe.dsl

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scalapipe._

class Kernel(val name: String) extends LowPriorityImplicits with DebugInfo {

    collectDebugInfo
    SymbolValidator.validate(name, this)

    private val labelCounter = new LabelMaker("S")
    private[scalapipe] val inputs = new ListBuffer[KernelInput]
    private[scalapipe] val outputs = new ListBuffer[KernelOutput]
    private[scalapipe] val states = new ListBuffer[KernelLocal]
    private[scalapipe] val configs = new ListBuffer[KernelConfig]
    private[scalapipe] val externals = new ListBuffer[Platforms.Value]
    private[scalapipe] val dependencies = new DependencySet
    private[scalapipe] val scopeStack = new ListBuffer[scalapipe.Scope]
    private[scalapipe] val verbatims = new ListBuffer[String]
    
    
    def this() = this(LabelMaker.getKernelLabel)

    implicit val im = this

    scopeStack += new scalapipe.Scope(this, NodeType.BLOCK)

    private[scalapipe] def getRoot: ASTNode = {
        DSLHelper.ifThen(scopeStack.size > 1,
                         Error.raise("missing end", this))
        scopeStack.last.handleEnd
    }

    private[scalapipe] def isInternal(p: Platforms.Value): Boolean = {
        !externals.contains(p)
    }

    private[dsl] def getLabel: String = labelCounter.next()

    // Temporary configuration functions
    private[scalapipe] def setstate(size : Int) {
        configs += new KernelConfig("statesize",ValueType.signed32,size)
    }
    private[scalapipe] def setcomp(comp : Int) {
        configs += new KernelConfig("computation",ValueType.signed32,comp)
    }
    
    def output(t: Type, n: Symbol = null, rate: Int = 1): Variable = {
        val vt = t.create
        DSLHelper.ifThen(!vt.flat, Error.raise("output port too wide", this))
        val label = DSLHelper.ifThenElse(n != null, n.name, getLabel)
        val node = new Variable(label, this)
        outputs += new KernelOutput(label, vt, rate)
        node
    }
    def verbatim(s : String) {
        verbatims += s
    }
    def input(t: Type, n: Symbol = null, rate: Int = 1): Variable = {
        val vt = t.create
        DSLHelper.ifThen(!vt.flat, Error.raise("input port too wide", this))
        val label = DSLHelper.ifThenElse(n != null, n.name, getLabel)
        val node = new Variable(label, this)
        inputs += new KernelInput(label, vt, rate)
        node
    }

    def config(t: Type, n: Symbol, v: Any = null): Variable = {
        val node = new Variable(n.name, this)
        configs += new KernelConfig(n.name, t.create(), v)
        node
    }

    def external(platform: String, file: String = name) {
        DSLHelper.ifThenElse(platform.equals("all"), {
            externals ++= Platforms.values
        }, {
            externals += Platforms.withName(platform, this)
        })
    }

    def local(t: Type, v: Any = null): Variable = {
        val label = getLabel
        states += new KernelLocal(label, t.create(), v)
        new Variable(label, this)
    }

    private[scalapipe] def local(vt: ValueType): Variable = {
        val label = getLabel
        states += new KernelLocal(label, vt)
        new Variable(label, this)
    }

    def cast(expr: ASTNode, t: Type): ASTNode = {
        ASTConvertNode(expr, t.create(), this)
    }

    def __ifThenElse[T](cond: Boolean, thenp: => T, elsep: => T) {
        DSLHelper.ifThenElse(cond, thenp, elsep)
    }

    def __ifThenElse[A <% ASTNode, T](cond: A, thenp: => T, elsep: => T) {
        scopeStack += new scalapipe.Scope(this, NodeType.IF, cond)
        thenp
        scopeStack.last.handleElse
        elsep
        val prev = scopeStack.last
        scopeStack.trimEnd(1)
        scopeStack.last += prev.handleEnd
    }

    def __whileDo[A <% ASTNode, T](cond: A, body: => T) {
        scopeStack += new scalapipe.Scope(this, NodeType.WHILE, cond)
        body
        val prev = scopeStack.last
        scopeStack.trimEnd(1)
        scopeStack.last += prev.handleEnd
    }

    def __doWhile[A <% ASTNode, T](body: => T, cond: A) {
        body
        scopeStack += new scalapipe.Scope(this, NodeType.WHILE, cond)
        body
        val prev = scopeStack.last
        scopeStack.trimEnd(1)
        scopeStack.last += prev.handleEnd
    }

    def __equal[A <% ASTNode, B <% ASTNode](a: A, b: B): ASTNode = {
        ASTOpNode(NodeType.eq, a, b, this)
    }

    def __notEqual[A <% ASTNode, B <% ASTNode](a: A, b: B): ASTNode = {
        ASTOpNode(NodeType.ne, a, b, this)
    }


    def __assign[A <% ASTSymbolNode, B <% ASTNode](a: A, b: B): ASTNode = {
        ASTAssignNode(a, b, this)
    }

    def switch[A <% ASTNode, T](cond: A)(cases: => T) {
        scopeStack += new scalapipe.Scope(this, NodeType.SWITCH, cond)
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

    def stop {
        ASTStopNode(this)
    }

    def abs[T <% ASTNode](s: T) = s.abs

    def exp[T <% ASTNode](s: T) = s.exp

    def log[T <% ASTNode](s: T) = s.log

    def sqrt[T <% ASTNode](s: T) = s.sqrt

    def avail(s: ASTSymbolNode) = ASTAvailableNode(s.symbol, this)

    def addr(n: ASTNode) = ASTOpNode(NodeType.addr, n, null, this)

    def sizeof(t: Type) = IntLiteral(t.create.bits / 8, this)

    def sizeof(n: ASTNode) = ASTOpNode(NodeType.sizeof, n, null, this)

    implicit def bool(b: Boolean) = IntLiteral(b, this)

    implicit def byte(b: Byte) = IntLiteral(b, this)

    implicit def char(c: Char) = IntLiteral(c, this)

    implicit def short(s: Short) = IntLiteral(s, this)

    implicit def int(i: Int) = IntLiteral(i, this)

    implicit def long(l: Long) = IntLiteral(l, this)

    implicit def float(f: Float) = FloatLiteral(f, this)

    implicit def double(d: Double) = FloatLiteral(d, this)

    implicit def func(f: Func) = {
        dependencies.add(f.dependencies)
        ASTCallNode(f, this)
    }

    implicit def string(s: String) = StringLiteral(s, this)

    implicit def builder(b: Variable) = b.create

    private[scalapipe] class ASTRange(
            val start: ASTNode,
            val stop: ASTNode,
            val step: ASTNode,
            val inclusive: Boolean) {

        def by(s: ASTNode): ASTRange =
            new ASTRange(start, stop, s, inclusive)

        def foreach(body: ASTNode => Unit) {
            val k = implicitly[Kernel]
            val sym = local(DSLHelper.getType(k, start))
            sym = start
            if (step > 0) {
                val cond = DSLHelper.ifThenElse(inclusive,
                                                sym <= stop,
                                                sym < stop)
                while (cond) {
                    body(sym)
                    sym += step
                }
            } else {
                val cond = DSLHelper.ifThenElse(inclusive,
                                                sym >= stop,
                                                sym > stop)
                while (cond) {
                    body(sym)
                    sym += step
                }
            }
        }

    }

}
