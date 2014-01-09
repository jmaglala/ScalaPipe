
package examples

import autopipe._
import autopipe.dsl._
import blocks._

object RandomTest extends App {

    val runLength  = 1e4     // Number of numbers to generate per round
    val rounds      = 1e4     // Number of rounds.
    val mode = 1                // 0 - CPU, 1 - FPGA, 2 - Custom FPGA

    object CustomMT19937 extends AutoPipeBlock("mt19937") {
        val state = input(UNSIGNED32, 'state)
        val out = output(UNSIGNED32, 'y)
        external("HDL")
    }

    val RNG = mode match {
            case 0 => MT19937
            case 1 => MT19937
            case 2 => CustomMT19937
        }

    val Sink = new AutoPipeBlock {

        val in = input(SIGNED32)
        val out = output(UNSIGNED8)
        val temp = local(UNSIGNED32)
        val count = local(UNSIGNED32, 0)

        temp = in
        count += 1
        if (count >= runLength) {
            out = 1
            count = 0
        }

    }

    val Terminate = new AutoPipeBlock {
        val in = input(UNSIGNED8)
        val temp = local(UNSIGNED8)
        val round = local(UNSIGNED32, 0)
        temp = in
        round += 1
        if (round >= rounds) {
            stdio.exit(0)
        }
    }

    val app = new AutoPipeApp {

        val gen = GenState()
        val random = RNG(gen)
        val normal = ZigguratNormal(random)
        val sink = Sink(normal)
        Terminate(sink)

        mode match {
            case 0 => ()
            case 1 | 2 =>
                map(GenState -> RNG, CPU2FPGA())
                map(Sink -> Terminate, FPGA2CPU())
        }

    }
    app.emit("random")

}

