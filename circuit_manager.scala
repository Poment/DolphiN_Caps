package circuit.manager

import chisel3._
import circuit.matrix._

/* CORE CIRCUIT */
class RecordController extends Module {
	val io = IO(new Bundle {
		val command = Input()
		val jobEnd = Input(Bool())
		val go = Output(Bool())
		val toDownRecord = Output()
		val toRightRecord = Output()
	})
	// record setting
	val downRecord =
		Vec(Seq.fill(m){ Module(new RECORD()).io })
	val rightRecord =
		Vec(Seq.fill(n){ Module(new RECORD()).io })
	// command processing
	when(?) {
		when(?) {
		} .elsewhen(?)
	}
}

/* Temp Record to insert input */
class Record extends Module {
	val io = IO(new Bundle {
		val get = Input(UInt(8.W))
		val toCell = Output(UInt(8.W))
		val toNext = Output(UInt(8.W))
	})
	// Initialize
	when(reset.toBool) {
		r0 := 0.U
		r1 := 0.U
	}
	// Keep Value
	val r0 = RegNext(io.get)
	val r1 = RegNext(r0)
	// Output
	io.toCell := r0
	io.toNext := r1
}
