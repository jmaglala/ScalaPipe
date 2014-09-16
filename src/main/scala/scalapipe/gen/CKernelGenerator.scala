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
         
        /*for (c <- kt.configs) {
            val cname = c.name
            val vtype = c.valueType
            write(s"$vtype $cname;")
        }*/
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
        write(s"int get_free(int out_port);")
        write(s"int get_available(int in_port);")
        write(s"char * read_value(int in_port);")
        write(s"bool fireable();")
        
        leave
        write(";")

        emitFunctionHeader
        write(s"#endif")

        getOutput

    }

    private def emitInit {

        val kname = kt.name
        val sname = s"sp_${kname}_data"
        
        write(s"${kname}::${kname}(int _in, int _out, int _state, int _rt) : Kernel(_in,_out,_state,_rt)")
        enter
        //write(s"super(_in,_out,_state,_rt);")
        write(s"${kt.name} * kernel = this;")
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
        write(s"${kname}::~${kname}()")
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
            //val ktype = s"sp_${kname}_data"
            write(s"SP_READ_FUNCTION($vtype, $index);")
        }

        write(s"void ${kname}::run()")
        enter
        //write(s"${kt.name} * kernel = this;")
        // Declare inputs
        for (i <- kt.inputs)
        {
            val name = i.name
            val vtype = i.valueType
            write(s"$vtype $name;")
        }
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
            write(s"$vtype $name;")
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

    private def emitGetFree {
        val kname = kt.name
        write(s"int ${kname}::get_free(int out_port)")
        enter
            write(s"int result = 0;")
            write(s"if (out_port <= outputs.size()-1)")
            enter
                write(s"result = outputs[out_port]->get_free();")
            leave
            write(s"return result;")
        leave
    }
    
    private def emitGetAvailable {
        val kname = kt.name
        write(s"int ${kname}::get_available(int in_port)")
        enter
            write(s"int result = 0;")
            write(s"if (in_port <= inputs.size()-1)")
            enter
                write(s"result = inputs[in_port]->get_available();")
            leave
            write(s"return result;")
        leave
    }
    
    private def emitReadValue {
        val kname = kt.name
        write(s"char * ${kname}::read_value(int in_port)")
        enter
            write(s"char *ptr = NULL;")
            write(s"int end_count = 0;")
            write(s"for(;;)")
            enter
                write(s"inputs[in_port]->read((char &) *ptr);")
                write(s"if(SPLIKELY(ptr != NULL))")
                enter
                    write(s"clock.count += 1;")
                    write(s"return ptr;")
                leave
            leave
        leave
    }
    
    private def emitFireable {
        val kname = kt.name
        write(s"bool ${kname}::fireable()")
        enter
            write(s"bool fireable = true;")
            write(s"if (outputs.size() > 0)")
            enter
                write(s"fireable = fireable && outputs[0]->ready(outrate,true);")
            leave
            write(s"if (inputs.size() > 0)")
            enter
                write(s"fireable = fireable && inputs[0]->ready(inrate,false);")
            leave
            write(s"return fireable;")
        leave
    }
    
    private def emitSource: String = {
        emitInit
        emitDestroy
        emitRun
        emitGetFree
        emitGetAvailable
        emitReadValue
        emitFireable
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
