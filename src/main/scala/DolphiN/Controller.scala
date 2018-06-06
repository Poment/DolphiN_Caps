package DolphiN

import Chisel._ // Should we use Chisel2?

import freechips.rocketchip.tile // Is the grammer right?

class Controller(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new ControllerModule(this)
}

// HasCoreParameteres? HasL1CacheParameters? Parameters? other parameters?
class ControllerModule(outer: Controller) extends LazyRoCCModule(outer) {
  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val doReport = funct === UInt(1)
  val doReserve = funct === UInt(2)
  val doSetParam = funct === UInt(4)
  val doSetSpParam = funct === UInt(8)
  val doStart = funct === UInt(16)
  val doReset = funct === UInt(32)

  // These below has to be modified in width for scalability?
  val regA = Reg(UInt(width = 8))
  val regB = Reg(UInt(width = 8))
  val regK = Reg(UInt(width = 8))
  val regXO0 = Reg(UInt(width = 8))
  val regXO1 = Reg(UInt(width = 8))
  val regYO0 = Reg(UInt(width = 8))
  val regYO1 = Reg(UInt(width = 8))
  val regIA0 = Reg(UInt(width = 64))
  val regIA1 = Reg(UInt(width = 64))
  val regOA = Reg(UInt(width = 64))
  val regMA = Reg(UInt(width = 64))
  val regBA = Reg(UInt(width = 64))
  val regM = Reg(UInt(width = 8))
  val regN = Reg(UInt(width = 8))

  when (cmd.fire() && doReport){

  }
  when (cmd.fire() && doReserve){
    regOA := io.cmd.bits.rs1
    regMA := io.cmd.bits.rs2

  }
  when (cmd.fire() && doSetParam){
    regA := 
    regB := 
    regK := 
    regXO0 := 
    regXO1 := 
    regYO0 := 
    regYO1 := 

  }
  when (cmd.fire() && doSetSpParam){
    regBA := 

  }
  when (cmd.fire() && doStart){
    regIA0 := 
    regIA1 := 

  }
  when (cmd.fire() && doReset){

  }
}
