package scalapipe

private[scalapipe] class ApplicationParameters extends Parameters {

    add('queueDepth, 256)
    add('fpgaQueueDepth, 1)
    add('defaultPlatform, "C")
    add('defaultHost, "localhost")
    add('timeTrialOutput, null: String)
    add('timeTrialBufferSize, 8192)
    add('timeTrialAffinity, -1)
    add('share, 1)              // Share FPGA resources within a kernel:
                                //  0 - no sharing
                                //  1 - share independent resources
                                //  2 - share all resources
    add('profile, false)        // Insert counters for profiling.
    add('fpga, "Simulation")    // Default FPGA device to target.
    add('trace, false)          // Set to generate address traces from C code.
    add('wave, false)           // Dump waveform from simulation.
    add('basePort, 9000)        // First port number to use.
    add('memoryAddrWidth, 30)   // FPGA memory address width.
    add('memoryWidth, 32)       // FPGA memory port width.
    add('bram, true)            // Set to use block RAM for memories.
    add('dramAddrWidth, 27)     // DRAM address width.
    add('dramDataWidth, 128)    // DRAM data width.
    add('sched, "EvenSeg")     // Scheduler type
    add('schedparam, 1)         // A parameter for the scheduler
    add('cores, 1)              // The number of cores to use
    add('baseAffinity, 8)       // The starting core id to use
    add('iterations, 0)         // The number of times the first kernel will issue
    add('cache, 1024*256)       // The size of the l2 cache in B
    
}
