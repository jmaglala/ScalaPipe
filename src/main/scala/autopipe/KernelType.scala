package autopipe

import autopipe.dsl._

private[autopipe] abstract class KernelType(
        val ap: AutoPipe,
        val name: String,
        val symbols: SymbolTable,
        val platform: Platforms.Value,
        val loopBack: Boolean
    ) {

    private[autopipe] val configs = symbols.configs
    private[autopipe] val states = symbols.states
    private[autopipe] val temps = symbols.temps
    private[autopipe] val parameters = ap.parameters
    private[autopipe] val dependencies = new DependencySet
    private[autopipe] val label = LabelMaker.getTypeLabel
    private[autopipe] val inputs = symbols.inputs
    private[autopipe] val outputs = symbols.outputs

    def this(ap: AutoPipe, apb: AutoPipeBlock, p: Platforms.Value) = {
        this(ap, apb.name, new SymbolTable(apb), p, apb.loopBack)
        apb.inputs.foreach { i =>
            symbols.addInput(i.name, i.valueType)
        }
        apb.outputs.foreach { o =>
            symbols.addOutput(o.name, o.valueType)
        }
        apb.configs.foreach { c =>
            symbols.addConfig(c.name, c.valueType, Literal.get(c.default, apb))
        }
        apb.states.foreach { s =>
            symbols.addState(s.name, s.valueType, Literal.get(s.init, apb))
        }
        dependencies.add(apb.dependencies)
    }

    private[autopipe] def getType(node: ASTSymbolNode): ValueType = {
        val name = node.symbol
        val vtype = symbols.getType(name)
        if (vtype == null) {
            Error.raise("symbol not declared: " + name, node)
            ValueType.void
        } else {
            vtype
        }
    }

    private[autopipe] def isInput(node: ASTSymbolNode): Boolean = {
        symbols.isInput(node.symbol)
    }

    private[autopipe] def isOutput(node: ASTSymbolNode): Boolean = {
        symbols.isOutput(node.symbol)
    }

    private[autopipe] def isInternal(f: AutoPipeFunction): Boolean = {
        f.isInternal(platform)
    }

    private[autopipe] def functions: Seq[AutoPipeFunction] = Seq()

    private[autopipe] def createTemp(vt: ValueType): TempSymbol =
        symbols.createTemp(vt)

    private[autopipe] def releaseTemp(t: BaseSymbol) {
        symbols.releaseTemp(t)
    }

    private[autopipe] def getSymbol(name: String): BaseSymbol =
        symbols.get(name)

    private[autopipe] def getBaseOffset(name: String): Int =
        symbols.getBaseOffset(name)

    def getLiteral(lit: Literal): String = lit match {
        case sl: SymbolLiteral =>
            configs.find(_.name == sl.symbol) match {
                case Some(v)    => getLiteral(v.value)
                case None       =>
                    Error.raise("config option not found: " + sl.symbol, this)
            }
        case _ => lit.toString
    }

    override def toString = name

    private[autopipe] def inputType(p: PortName): ValueType = {
        val s = symbols.getInput(p)
        if (s == null) {
            Error.raise("input port " + p + " not found", this)
        }
        s.valueType
    }

    private[autopipe] def outputType(p: PortName): ValueType = {
        val s = symbols.getOutput(p)
        if (s == null) {
            Error.raise("output port " + p + " not found", this)
        }
        s.valueType
    }

    private[autopipe] def inputIndex(p: PortName): Int = symbols.inputIndex(p)

    private[autopipe] def outputIndex(p: PortName): Int = symbols.outputIndex(p)

    def inputIndex(n: String): Int = inputIndex(new StringPortName(n))

    def outputIndex(n: String): Int = outputIndex(new StringPortName(n))

    private[autopipe] def emit(dir: java.io.File)

    private[autopipe] def internal: Boolean

    private[autopipe] def inputName(i: Int) = inputs(i).name

    private[autopipe] def outputName(i: Int) = outputs(i).name

    def isInput(n: String) = inputs.exists(_.name == n)

    def isOutput(n: String) = outputs.exists(_.name == n)

    def isPort(n: String) = isInput(n) || isOutput(n)

    def isState(n: String) = states.exists(_.name == n)

    def isConfig(n: String) = configs.exists(_.name == n)

    def isLocal(n: String) = symbols.get(n) match {
        case sn: StateSymbol => sn.isLocal
        case _                    => false
    }

}