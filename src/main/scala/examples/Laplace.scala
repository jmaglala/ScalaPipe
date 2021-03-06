package examples

import scalapipe.kernels._
import scalapipe.dsl._
import scala.math.log

object Laplace {


    val walkCount   = 16      // Number of walk blocks (power of 2).
    val mapping     = 2        // See below for descriptions.
    val customRNG   = false
    var width       = 100     // Width of the area
    var height      = 100     // Height of the area
    var iterations  = 256 / walkCount

    // Resource mappings:
    // 0: Everything on a single CPU.
    // 1: RNG on FPGA, everything else on a single CPU.
    // 2: RNG and walks on FPGA.
    // 3: Each walk on a seperate CPU.
    // 4: RNG on FPGA, each walk on a separate CPU.
    // 5: RNG and walks on GPU.

    def main(args: Array[String]) {

        val SplitU32 = new Split(UNSIGNED32)
        val AverageU32 = new Average(UNSIGNED32)

        object ExternalMT19937 extends Kernel("mt19937") {
            val state = input(UNSIGNED32, 'state)
            val out = output(UNSIGNED32, 'y)
            external("HDL")
        }

        val RNG = if (customRNG) ExternalMT19937 else MT19937

        val Walk = new Kernel {

            val x0 = input(UNSIGNED32)
            val y0 = output(UNSIGNED32)

            val xstart = local(SIGNED32, 0)
            val ystart = local(SIGNED32, 0)
            val xcoord = local(SIGNED32, 0)
            val ycoord = local(SIGNED32, 0)
            val iter = local(UNSIGNED32, 0)
            val total = local(UNSIGNED64, 0)

            val random = local(UNSIGNED32)
            val dir = local(UNSIGNED32)
            val i = local(UNSIGNED32)
            val temp = local(UNSIGNED32)
            val atBoundary = local(UNSIGNED8)

            random = x0
            i = 0
            while (i < 16) {

                temp = 0
                dir = random & 3
                switch(dir) {
                    when(0) {
                        xcoord += 1
                        atBoundary = xcoord >= width
                    }
                    when(1) {
                        xcoord -= 1
                        temp = 100
                        atBoundary = xcoord < 0
                    }
                    when(2) {
                        ycoord += 1
                        atBoundary = ycoord >= height
                    }
                    when(3) {
                        ycoord -= 1
                        atBoundary = ycoord < 0
                    }
                }

                if (atBoundary) {
                    total += temp
                    iter += 1
                    if (iter == iterations) {
                        y0 = total / iterations
                        total = 0
                        iter = 0
                        xstart += 1
                        if (xstart == width) {
                            xstart = 0
                            ystart += 1
                        }
                    }
                    xcoord = xstart
                    ycoord = ystart
                }

                random >>= 2
                i += 1
            }

        }

        val Print = new Kernel("Print") {

            val x0 = input(UNSIGNED32)

            val filename = config(STRING, 'filename, "results.txt")

            val fd = local(stdio.FILEPTR, 0)
            val x = local(UNSIGNED32, 0)
            val y = local(UNSIGNED32, 0)

            if (fd == 0) {
                fd = stdio.fopen(filename, "w")
                if (fd == 0) {
                    stdio.printf("Could not open %s\n", filename)
                }
            }

            stdio.fprintf(fd, "%u ", x0)
            x += 1
            if (x == width) {
                stdio.fprintf(fd, "\n")
                stdio.fflush(fd)
                x = 0
                y += 1
                if (y == height) {
                    stdio.exit(0)
                }
            }

        }

        val Laplace = new Application {

            // Determine the number of levels of splits we need.
            val levels = (log(walkCount) / log(2)).toInt

            // Set up the RNG state and walk configuration.
            val state = MT19937State()
            val random = RNG(state)

            // Set up the split blocks.
            val splits = random.iteratedMap(levels, SplitU32)

            // Set up the walk blocks.
            val walks = Array.tabulate(walkCount)(x => {
                Walk(splits(x))()
            })

            // Set up the average blocks.
            val result = iteratedFold(walks, AverageU32)

            // Set up the print block.
            Print(result)

            // Resource mapping.
            mapping match {
            case 0 => {     // Everything on a single CPU.
                }
            case 1 => {     // RNG on FPGA, everything else on a single CPU.
                    map(ANY_KERNEL -> RNG, CPU2FPGA())
                    map(RNG -> ANY_KERNEL, FPGA2CPU())
                }
            case 2 => {     // RNG and walks on FPGA.
                    map(ANY_KERNEL -> RNG, CPU2FPGA())
                    map(ANY_KERNEL -> Print, FPGA2CPU())
                }
            case 3 => {     // Each walk on a seperate CPU.
                    map(ANY_KERNEL -> Walk, CPU2CPU(id = -1))
                    map(Walk -> ANY_KERNEL, CPU2CPU(id = 0))
                }
            case 4 => {     // RNG on FPGA, each walk on a separate CPU.
                    map(ANY_KERNEL -> RNG, CPU2FPGA())
                    map(RNG -> ANY_KERNEL, FPGA2CPU())
                    map(ANY_KERNEL -> Walk, CPU2CPU(id = -1))
                    map(Walk -> ANY_KERNEL, CPU2CPU(id = 0))
                }
            case 5 => {     // RNG and walks on GPU.
                    map(ANY_KERNEL -> RNG, CPU2GPU())
                    map(ANY_KERNEL -> Print, GPU2CPU())
                }
            }

        }
        Laplace.emit("laplace")

    }

}

