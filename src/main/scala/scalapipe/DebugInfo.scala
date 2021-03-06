package scalapipe

private[scalapipe] trait DebugInfo {

    private[scalapipe] var fileName: String = null
    private[scalapipe] var lineNumber = 0

    def collectDebugInfo {
        try {
            throw new Exception("DEBUG")
        } catch {
            case e: Exception =>
                val trace = e.getStackTrace.filter { i =>
                    !i.getClassName().startsWith("scalapipe.") ||
                    i.getClassName.startsWith("scalapipe.test.")
                }
                if (!trace.isEmpty) {
                    fileName = trace.head.getFileName
                    lineNumber = trace.head.getLineNumber
                }
        }
    }

}
