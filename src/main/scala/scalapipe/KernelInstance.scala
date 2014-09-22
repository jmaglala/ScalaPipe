package scalapipe

import scalapipe.dsl.Kernel

private[scalapipe] class KernelInstance(
        val sp: ScalaPipe,
        val kernel: Kernel
    ) extends DebugInfo {

    private[scalapipe] val name = kernel.name
    private[scalapipe] val index = LabelMaker.getInstanceIndex
    private[scalapipe] val label = s"instance$index"
    private[scalapipe] var device: Device = null
    private[this] var outputs = Map[IntPortName, Stream]()
    private[this] var inputs = Map[IntPortName, Stream]()
    private[scalapipe] var configs = Map[String, Literal]()
    private[scalapipe] var state: Int = 1
    collectDebugInfo

    def apply(): StreamList = new StreamList(sp, this)

    def apply(s: Stream): StreamList = {
        setInput(null, s)
        new StreamList(sp, this)
    }

    def apply(args: (Symbol, Any)*) = {
        for(a <- args) {
            val name = if (a._1 == null) null else a._1.name
            if(a._2.isInstanceOf[Stream]) {
                setInput(name, a._2.asInstanceOf[Stream])
            } else if(name != null) {
                setConfig(name, a._2)
            }
        }
        new StreamList(sp, this)
    }

    def apply(args: Array[Stream]) = {
        for (a <- args) setInput(null, a)
        new StreamList(sp, this)
    }

    private[scalapipe] def kernelType = sp.kernelType(name, device.platform)

    private[this] def setInput(n: String, s: Stream) {
        val portName = if (n == null) {
                new IntPortName(inputs.size)
            } else {
                new StringPortName(n)
            }
        if (!isInput(portName)) {
            Error.raise(s"invalid input port: $portName", this)
        }
        val portIndex = new IntPortName(inputIndex(portName))
        if (inputs.contains(portIndex)) {
            Error.raise(s"input connected multiple times: $portName", this)
        }
        s.setDest(this, portIndex)
        inputs += (portIndex -> s)
    }

    private[scalapipe] def setOutput(pn: PortName, s: Stream) {
        if (!isOutput(pn)) {
            Error.raise(s"invalid output port: $pn", this)
        }
        val portIndex = new IntPortName(outputIndex(pn))
        if (outputs.contains(portIndex)) {
            Error.raise(s"output connected multiple times: $pn", this)
        }
        outputs += (portIndex -> s)
    }

    private[scalapipe] def replaceInput(os: Stream, ns: Stream) {
        for (i <- inputs if i._2 == os) {
            val portName = i._1
            inputs += (portName -> ns)
            ns.setDest(this, portName)
            return
        }
        sys.error("internal error: old stream not found")
    }

    private[scalapipe] def setConfig(n: String, v: Any) {
        val value = Literal.get(v)
        configs += (n -> value)
    }

    private[scalapipe] def setConfigs(o: KernelInstance) {
        configs ++= o.configs
    }

    private[scalapipe] def getConfig(n: String): Literal = {
        configs.get(n) match {
            case Some(v) => v
            case None => null
        }
    }

    private[this] def getPort(pn: PortName,
                              lst: Seq[KernelPort]): Option[KernelPort] = {
        pn match {
            case ip: IntPortName if ip.name < lst.size => Some(lst(ip.name))
            case _ => lst.find(_.name == pn)
        }
    }

    private[this] def getInput(pn: PortName) = getPort(pn, kernel.inputs)

    private[this] def getOutput(pn: PortName) = getPort(pn, kernel.outputs)

    private[scalapipe] def inputName(pn: PortName) = getInput(pn) match {
        case Some(i)    => i.name
        case _          => null
    }

    private[scalapipe] def inputType(pn: PortName) = getInput(pn) match {
        case Some(i)    => i.valueType
        case _          => ValueType.void
    }

    private[scalapipe] def outputName(pn: PortName) = getOutput(pn) match {
        case Some(o)    => o.name
        case _          => null
    }

    private[scalapipe] def outputType(pn: PortName) = getOutput(pn) match {
        case Some(o)    => o.valueType
        case _          => ValueType.void
    }

    private[this] def isInput(pn: PortName) = !getInput(pn).isEmpty

    private[this] def isOutput(pn: PortName) = !getOutput(pn).isEmpty

    private[scalapipe] def inputIndex(pn: PortName): Int = pn match {
        case in: IntPortName => in.name
        case _ => kernel.inputs.indexWhere(_.name == pn)
    }

    private[scalapipe] def inputIndex(s: Stream): Int = {
        val matches = inputs.toSeq.filter { case (k, v) => v == s }
        inputIndex(matches.head._1)
    }

    private[scalapipe] def outputIndex(pn: PortName): Int = pn match {
        case in: IntPortName => in.name
        case _ => kernel.outputs.indexWhere(_.name == pn)
    }

    private[scalapipe] def outputIndex(s: Stream): Int = {
        val matches = outputs.toSeq.filter { case (k, v) => v == s }
        outputIndex(matches.head._1)
    }

    private[scalapipe] def getInputs: Seq[Stream] = inputs.toSeq.map(_._2)

    private[scalapipe] def getOutputs: Seq[Stream] = outputs.toSeq.map(_._2)

    override def toString = name

    private[scalapipe] def validate {

        if (inputs.size > kernel.inputs.size) {
            Error.raise("too many inputs connected for " + name, this)
        } else if (inputs.size < kernel.inputs.size) {
            Error.raise("too few inputs connected for " + name, this)
        }

        if (outputs.size > kernel.outputs.size) {
            Error.raise("too many outputs connected for " + name, this)
        } else if (outputs.size < kernel.outputs.size) {
            Error.raise("too few outputs connected for " + name, this)
        }

    }

}
