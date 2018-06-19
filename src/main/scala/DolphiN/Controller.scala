package DolphiN

import Chisel._ // Should we use Chisel2?

import freechips.rocketchip.tile // Is the grammer right?

class Controller(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new ControllerModule(this)
}

// HasCoreParameteres? HasL1CacheParameters? Parameters? other parameters?
class ControllerModule(outer: Controller, m: Int) extends LazyRoCCModule(outer) {
  val systolicArray = new RecordController(m)

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val doReport = funct === UInt(0)
  val doReserve = funct === UInt(1)
  val doSetParam = funct === UInt(2)
  val doStart = funct === UInt(3)
  val doRelease = funct === UInt(4)

  // These below has to be modified in width for scalability?
  val regReserve = Reg(init = Uint(0))
  val regStart = Reg(init = Uint(0))
  val regLoad = Reg(init = Uint(0))
  val regAddr = Reg(UInt(width = 64))
  val regOffset = Reg(init = UInt(0, width = 16))
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
    when (regRes){

    }
    .elsewhen (regRes){

    }
    .otherwise{

    }
  }
  .elsewhen (cmd.fire() && doReserve){
    regReserve := UInt(1)
    regOA := io.cmd.bits.rs1
    regMA := io.cmd.bits.rs2
  }
  .elsewhen (cmd.fire() && doSetParam){
    regA := io.cmd.bits.rs1(15,8)
    regB := io.cmd.bits.rs1(23,16)
    regK := io.cmd.bits.rs1(31,24)
    regXO0 := io.cmd.bits.rs2(7,0)
    regXO1 := io.cmd.bits.rs2(15,8)
    regYO0 := io.cmd.bits.rs2(23,15)
    regYO1 := io.cmd.bits.rs2(31,24)
  }
  .elsewhen (cmd.fire() && doStart){
    regStart := UInt(1)
    regAddr := regIA0 + regOffset
    io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
    io.mem.req.bits.addr := regAddr
    io.mem.req.bits.tag := 
    io.mem.req.bits.cmd := M_XRD
    io.mem.req.bits.typ := MT_D
    io.mem.req.bits.data := Bits(0)
    io.mem.req.bits.phys := Bool(false)
  }
  .elsewhen (regStart){
    regLoad = UInt(1)
    io.mem.req.bits.addr := regIA0
    reg = regIA0 + 8
  }
  .elsewhen (regLoad){
    regIA0 = regIA0 + 1
  }
  .elsewhen (cmd.fire() && doRelease){
    regReserve := UInt(0)
    regStart := UInt(0)
    regLoad := UInt(0)
  }
}
