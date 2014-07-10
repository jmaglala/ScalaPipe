package scalapipe.gen

import java.io.File

import scalapipe._
import scalapipe.opt.IROptimizer

private[scalapipe] class CKernelGenerator(
        _kt: InternalKernelType
    ) extends KernelGenerator(_kt) with CGenerator with ASTUtils {

    protected def emitFunctionHeader {
    }

    protected def emitFunctionSource {
    }

    private def emitHeader: String = {

        val kname = s"${kt.name}"
        //val sname = s"struct ${kname}_data"

        write(s"#ifndef ${kname}_H_")
        write(s"#define ${kname}_H_")
        write("#include \"ScalaPipe.h\"")
        write("#include \"Kernel.h\"")
        kt.dependencies.get(DependencySet.Include).foreach { i =>
            write(s"#include <$i>")
        }

        // ????
        /*val typeEmitter = new CTypeEmitter
        kt.configs.foreach { c => typeEmitter.emit(c.valueType) }
        kt.states.foreach { s => typeEmitter.emit(s.valueType) }
        kt.inputs.foreach { i => typeEmitter.emit(i.valueType) }
        kt.outputs.foreach { o => typeEmitter.emit(o.valueType) }
        write(typeEmitter)
        */
        
        write(s"class ${kname} : public Kernel")
        enter
        write("int * state_buffer;")
         
        for (c <- kt.configs) {
            val cname = c.name
            val vtype = c.valueType
            write(s"$vtype $cname;")
        }
        for (s <- kt.states if !s.isLocal) {
            val sname = s.name
            val vtype = s.valueType
            write(s"$vtype $sname;")
        }
        if (kt.parameters.get[Boolean]('profile)) {
            write(s"unsigned long sp_clocks;")
        }
        if (kt.parameters.get[Boolean]('trace)) {
            val streamCount = kt.inputs.size + kt.outputs.size
            write(s"FILE *trace_fd;")
            write(s"int trace_streams[$streamCount];")
        }
        
        
        write(s"public:")
        
        write(s"${kname}(int, int, int, int);")
        write(s"~${kname}();")
        write(s"void run();")
        
        leave
        write(";")

        emitFunctionHeader
        write(s"#endif")

        getOutput

    }

    private def emitInit {

        val kname = kt.name
        val sname = s"sp_${kname}_data"
        
        write(s"void ${kname}::${kname}(int _in, int _out, int _state, int _rt)")
        enter
        write(s"super(_in,_out,_state,_rt);")
        write(s"void * kernel = this")
        for (s <- kt.states if !s.isLocal && s.value != null) {
            val field = s.name
            val value = kt.getLiteral(s.value)
            write(s"kernel->$field = $value;")
        }
        if (kt.parameters.get('profile)) {
            // We initialize the clocks to one to account for the start state.
            write(s"kernel->sp_clocks = 1;")
        }
        leave
    }

    private def emitDestroy {
        val kname = kt.name
        write(s"void ${kname}::~${kname}()")
        enter
        leave
    }

    private def emitRun {

        val kname = kt.name
        val timing: Map[ASTNode, Int] =
            if (kt.expression.pure && kt.parameters.get('profile)) {
                val ir              = IRNodeEmitter(kt).emit(kt.expression)
                val context         = new ProfileIRContext(kt)
                val graph           = IROptimizer(kt, context).optimize(ir)
                val moduleEmitter   = new HDLModuleEmitter(kt, graph)
                HDLTiming.computeAST(graph)
            } else null

        // Create input functions.
        for (i <- kt.inputs) {
            val index = i.id
            val vtype = i.valueType
            val ktype = s"sp_${kname}_data"
            write(s"SP_READ_FUNCTION($vtype, $ktype, $index);")
        }

        write(s"void ${kname}::run()")
        enter
        write(s"void * kernel = this")
        // Declare locals.
        for (l <- kt.states if l.isLocal) {
            val name = l.name
            val vtype = l.valueType
            write(s"$vtype $name;")
        }

        // Declare outputs.
        for (o <- kt.outputs) {
            val name = o.name
            val vtype = o.valueType
            write(s"$vtype *$name;")
        }

        // Write out the verbatim code
        for ( s <- kt.verbatims)
        {
            write(s)
        }
        
        // Generate the code.
        val nodeEmitter = new CKernelNodeEmitter(kt, timing)
        nodeEmitter.emit(kt.expression)
        //write(s"for(;;)")
        //enter
        write(nodeEmitter)
        //leave
        leave

    }

    private def emitSource: String = {
        emitInit
        emitDestroy
        emitRun
        emitFunctionSource
        getOutput
    }

    override def emit(dir: File) {

        import java.io.{FileOutputStream, PrintStream}

        // Create a directory for the kernel.
        val parent = new File(dir, kt.name)
        parent.mkdir

        // Generate the header
        val headerFile = new File(parent, s"${kt.name}.h")
        val headerPS = new PrintStream(new FileOutputStream(headerFile))
        headerPS.print(emitHeader)
        headerPS.close

        // Generate the source.
        val sourceFile = new File(parent, s"${kt.name}.cpp")
        val sourcePS = new PrintStream(new FileOutputStream(sourceFile))
        sourcePS.print("#include \"" + kt.name + ".h\"\n")
        sourcePS.print(emitSource)
        sourcePS.close

    }

}
