package scalapipe.dsl

import scalapipe.{KernelInstance}

class SPSegment (_id: Int) {
    private[scalapipe] var kernels = Seq[KernelInstance]()

    private[scalapipe] val id = _id
    private[scalapipe] var tid = 0
    
    private[scalapipe] var input_rate: Double = 1
    private[scalapipe] var output_rate: Double = 1
    private[scalapipe] var amplification: Double = 1
    private[scalapipe] var runtime = 0
    private[scalapipe] var state = 0
    
    def initVariables() {
        //set input rate
        if (kernels.head.kernel.inputs.length != 0)
            input_rate = kernels.head.kernel.inputs(0).rate
        else
            input_rate = -1
        
        //set output rate
        for (kernel <- kernels) {
            if (kernel != kernels.head)
                output_rate = output_rate / kernel.kernel.inputs(0).rate
            if (kernel.kernel.outputs.length != 0)
                    output_rate = output_rate * kernel.kernel.outputs(0).rate
            else
                output_rate = -1
        }
            
        //if middle segment, set amplification
        if (input_rate != -1 && output_rate != -1)
            amplification = output_rate / input_rate
            
        //iterate through and add up runtimes and state size
        for (kernel <- kernels) {
            runtime += kernel.kernelType.configs.filter(c => c.name == "runtime").head.value.long.toInt
            state += kernel.kernelType.configs.filter(c => c.name == "state").head.value.long.toInt
        }
        
        println("in:" + input_rate + " out:" + output_rate + " amp:" + amplification + " run:" + runtime + " state:" + state)
        //println()
        return
    }
}