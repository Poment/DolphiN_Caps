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
	// ==================================================
	// Modules
	// record setting
	val downRecord =
		Vec(Seq.fill(m){ Module(new Record()).io })
	val rightRecord =
		Vec(Seq.fill(n){ Module(new Record()).io })
	// Element setting
	val Elements = Vec(Seq.fill(m * n){ Module(new Element()).io })
	// Storage setting
	val TheStorage = Module(new Storage())
	// Wires
	// Elements - Storages
	for(i <- 0 until m * n) {
		TheStorage.io.frombuffers(i) := Elements(i).io.toBuffer
	}
	// Between Records and Record - Controller
	for(i <- 0 until m-1) {
		downRecord(i+1).io.get := downRecord(i).io.toNext
	}
	downRecord(0).io.get := io.toDownRecord
	for(i <- 0 until n-1) {
		rightRecord(i+1).io.get := rightRecord(i).io.toNext
	}
	rightRecord(0).io.get := io.toRightRecord
	// ==================================================
	// command processing
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
