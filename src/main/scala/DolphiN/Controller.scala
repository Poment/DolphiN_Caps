package DolphiN

import chisel3._
import freechips.rocketchip.tile // Is the grammer right?

//

//

class Controller(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new ControllerModule(this)
}

// HasCoreParameteres? HasL1CacheParameters? Parameters? other parameters?
class ControllerModule(outer: Controller) extends LazyRoCCModule(outer) {
  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct

}
