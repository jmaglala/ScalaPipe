package scalapipe.gen

import scalapipe._

/** Trait for outputing address traces. */
trait CTrace extends CNodeEmitter with ASTUtils {

    def emitSymbol(node: ASTSymbolNode): String

    def emitSymbolBase(node: ASTSymbolNode): String

    def write(code: String): Unit

    private def getOffset(node: ASTSymbolNode): String = {
        val location = "(char*)&" + emitSymbol(node)
        val base = "(char*)&" + emitSymbolBase(node)
        val baseOffset = kt.getBaseOffset(node.symbol)
        return s"((unsigned)($location - $base) + $baseOffset)"
    }

    override def emit(node: ASTNode) {

        if (kt.parameters.get[Boolean]('trace)) {

            // Trace inputs.
            for (index <- localInputs(node).map(kt.inputIndex)) {
                write(s"""fprintf(kernel->trace_fd, \"C$index\\n\");""")
            }

            // Trace reads.
            for (src <- localSources(node) if !kt.getType(src).flat) {
                val offsetStr = getOffset(src)
                val size = src.valueType.bytes.toHexString
                write("fprintf(kernel->trace_fd, \"R%x:" + size +
                      "\\n\", " + offsetStr + ");")
            }

            // Trace writes.
            for (dest <- localDests(node) if !kt.getType(dest).flat) {
                val offsetStr = getOffset(dest)
                val size = dest.valueType.bytes.toHexString
                write("fprintf(kernel->trace_fd, \"W%x:" + size +
                      "\\n\", " + offsetStr + ");")
            }

            // Trace outputs.
            for (index <- localOutputs(node).map(kt.outputIndex)) {
                write(s"""fprintf(kernel->trace_fd, \"P$index\\n\");""")
            }

        }

        super.emit(node)

    }

}