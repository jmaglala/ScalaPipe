
package autopipe.gen

import autopipe._

private[autopipe] class OpenCLBlockNodeEmitter(val bt: InternalBlockType,
                                                              val gen: StateTrait,
                                                              _timing: Map[ASTNode, Int])
    extends CNodeEmitter(bt, _timing) with CLike {

    private def setState(state: Int): String =
        "control->ap_state_index = " + state + ";"

    private def canWrite(name: String, index: Int): String =
        "(control->" + name + "_sent < control->" + name + "_size)"

    private def canRead(name: String, index: Int): String =
        "(control->" + name + "_read < control->" + name + "_size)"

    private def allocate(t: ValueType, name: String, index: Int): String = ""

    private val allocateBlocks = false

    private def inputAvailable(name: String): String = {
        "(control->" + name + "_size > control->" + name + "_read)"
    }

    private def outputFull(name: String): String = {
        "control->" + name + "_size == control->" + name + "_sent"
    }

    private def send(name: String, index: Int): String = {
        "barrier(CLK_GLOBAL_MEM_FENCE); " +
        "if(id == 0) { " + 
        "control->" + name + "_sent += unit_size; " +
        "} " +
        "barrier(CLK_GLOBAL_MEM_FENCE);"
    }

    private def release(name: String, index: Int, count: Int): String = {
        if (count > 0) {
            "barrier(CLK_GLOBAL_MEM_FENCE); " +
            "if(id == 0) { " +
            "control->" + name + "_read += " + count + " * unit_size; " +
            "} " +
            "barrier(CLK_GLOBAL_MEM_FENCE); "
        } else {
            ""
        }
    }

    private def ret(result: Int): String = {
        "control->ap_ready = 1; " +
        "return;"
    }

    private def derefInput(name: String): String =
        name + "[control->" + name + "_read + id]"

    private def derefOutput(name: String): String =
        name + "[control->" + name + "_sent + id]"

    override def emitAvailable(node: ASTAvailableNode): String = {
        val name = node.symbol
        if (bt.isInput(name)) {
            val index = bt.inputIndex(name)
            canRead(name, index)
        } else if (bt.isOutput(name)) {
            val index = bt.outputIndex(name)
            canWrite(name, index)
        } else {
            Error.raise("argument to avail must be an input or output", node)
        }
    }

    override def emitSymbol(node: ASTSymbolNode): String = {

        def isNative(vt: ValueType) = vt.isInstanceOf[NativeValueType]

        def isNativePointer(vt: ValueType) = vt match {
            case p: PointerValueType if isNative(p.itemType) => true
            case _ => false
        }

        val name = node.symbol
        if (node.index == null) {
            if (bt.isInput(name)) {
                derefInput(name)
            } else if (bt.isOutput(name)) {
                derefOutput(name)
            } else if (bt.isLocal(name)) {
                name
            } else if (bt.isState(name) || bt.isConfig(name)) {
                "block->" + name
            } else {
                Error.raise("symbol not declared: " + name, node)
            }
        } else if (isNative(node.valueType)) {
            if (node.index.isInstanceOf[SymbolLiteral]) {
                val indexString = node.index.toString
                if (bt.isInput(name) || bt.isOutput(name)) {
                    name + "." + indexString
                } else if (bt.isLocal(name)) {
                    name + "." + indexString
                } else if (bt.isState(name) || bt.isConfig(name)) {
                    "block->" + name + "." + indexString
                } else {
                    Error.raise("symbol not declared: " + name, node)
                }
            } else {
                val indexString = emitExpr(node.index)
                if (bt.isInput(name) || bt.isOutput(name)) {
                    name + "[" + indexString + "]"
                } else if (bt.isLocal(name)) {
                    name + "[" + indexString + "]"
                } else if (bt.isState(name) || bt.isConfig(name)) {
                    "block->" + name + "[" + indexString + "]"
                } else {
                    Error.raise("symbol not declared: " + name, node)
                }
            }
        } else if (isNativePointer(node.valueType)) {
            if (node.index.isInstanceOf[SymbolLiteral]) {
                val indexString = node.index.toString
                if (bt.isInput(name) || bt.isOutput(name)) {
                    name + "->" + indexString
                } else if (bt.isLocal(name)) {
                    name + "->" + indexString
                } else if (bt.isState(name) || bt.isConfig(name)) {
                    "block->" + name + "->" + indexString
                } else {
                    Error.raise("symbol not declared: " + name, node)
                }
            } else {
                val indexString = emitExpr(node.index)
                if (bt.isInput(name) || bt.isOutput(name)) {
                    "(*" + name + ")[" + indexString + "]"
                } else if (bt.isLocal(name)) {
                    "(*" + name + ")[" + indexString + "]"
                } else if (bt.isState(name) || bt.isConfig(name)) {
                    "(*block->" + name + ")[" + indexString + "]"
                } else {
                    Error.raise("symbol not declared: " + name, node)
                }
            }
        } else {
            if (node.index.isInstanceOf[SymbolLiteral]) {
                val indexString = emitExpr(node.index)
                if (bt.isInput(name) || bt.isOutput(name)) {
                    name + "." + indexString
                } else if (bt.isLocal(name)) {
                    name + "." + indexString
                } else if (bt.isState(name) || bt.isConfig(name)) {
                    "block->" + name + "." + indexString
                } else {
                    Error.raise("symbol not declared: " + name, node)
                }
            } else {
                val indexString = emitExpr(node.index)
                if (bt.isInput(name) || bt.isOutput(name)) {
                    name + ".values[" + indexString + "]"
                } else if (bt.isLocal(name)) {
                    name + ".values[" + indexString + "]"
                } else if (bt.isState(name) || bt.isConfig(name)) {
                    "block->" + name + ".values[" + indexString + "]"
                } else {
                    Error.raise("symbol not declared: " + name, node)
                }
            }
        }
    }

    override def emitAssign(node: ASTAssignNode) {

        var outputs = getLocalOutputs(node)
        for (o <- outputs) {
            val oindex = bt.outputIndex(o)
            val valueType = bt.outputs(oindex).valueType
            if (!allocateBlocks) {
                writeLeft("AP_STATE_" + gen.nextState + ":")
                write("if(" + outputFull(o) + ") {")
                enter
                write(setState(gen.currentState))
                write(ret(0))
                leave
                write("}")
            }
            write(allocate(valueType, o, oindex))
        }

        write(emitExpr(node.dest) + " = " + emitExpr(node.src) + ";")

        updateClocks(getTiming(node))

        for (o <- outputs) {
            val oindex = bt.outputIndex(o)
            write(send(o, oindex))
        }

    }

    override def emitStop(node: ASTStopNode) {
        updateClocks(getTiming(node))
        write(setState(-1))
        write(ret(1))
    }

    override def emitReturn(node: ASTReturnNode) {
        val output = "output"
        val valueType = bt.outputs(0).valueType
        if (!allocateBlocks) {
            writeLeft("AP_STATE_" + gen.nextState + ":")
            write("if(" + outputFull(output) + ") {")
            enter
            write(setState(gen.currentState))
            write(ret(0))
            leave
            write("}")
        }
        write(allocate(valueType, output, 0))
        write("*output = " + emitExpr(node.a) + ";")
        updateClocks(getTiming(node))
        write(send(output, 0))
    }

    override def updateClocks(count: Int) {
        if (count > 0) {
            write("block->ap_clocks += " + count + ";")
        }
    }

    override def checkInputs(node: ASTNode): Int = {
        beginScope
        val portsToCheck = getBlockingInputs(node).filter { !isCheckedPort(_) }
        if (!portsToCheck.isEmpty)  {
            addCheckedPorts(portsToCheck)
            writeLeft("AP_STATE_" + gen.nextState + ":")
            write("if(" + portsToCheck.map { inputAvailable(_) }.mkString(" && ") +
                    ") {")
            enter
        }
        gen.currentState
    }

    override def releaseInputs(node: ASTNode, state: Int) {
        val portsToRelease = getCheckedPorts
        endScope
        val inputs = bt.inputs.map({ _.name }).filter({ !isCheckedPort(_) })
        if (!portsToRelease.isEmpty) {
            for (i <- inputs) {
                val index = bt.inputIndex(i)
                if (portsToRelease.contains(i)) {
                    write(release(i, index, 1))
                } else {
                    // FIXME: this is wasteful and may not be necessary.
                    // We need to ensure that we get called back if we return
                    // without consuming data for another port.
                    write("if(" + i + " != 0) {")
                    enter
                    write(release(i, index, 0))
                    leave
                    write("}")
                }
            }
            write(setState(0))
            leave
            write("} else {")
            enter
            write(setState(state))
            write(ret(0))
            leave
            write("}")
        }
    }

}
