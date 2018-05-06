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
	// Modules =========================================================
	// record setting
	val rightRecord =
		Vec(Seq.fill(m){ Module(new Record(m)).io })
	val downRecord =
		Vec(Seq.fill(n){ Module(new Record(n)).io })
	// Element setting
	val Elements = Vec(Seq.fill(m * n){ Module(new Element()).io })
	// Storage setting
	val TheStorage = Module(new Storage())
	// Wires ===========================================================
	// Elements - Storages
	for(i <- 0 until m * n) {
		TheStorage.io.frombuffers(i) := Elements(i).io.toBuffer
	}
	// Record - Contorller?

	// Element - Element
	// > Left - Right
	for(j <- 0 until m) {
		for(i <- 0 until n-1) {
			Elements(i+1+(j*n)).io.fromLeft := Elements(i+(j*n)).io.toRight
		}
	}
	// > Top - Down
	for(j <- 0 until m-1) {
		for(i <- 0 until n) {
			Elements(i+n).io.fromTop := Elements(i).io.toDown
		}
	}
	// Record - Element?
	for(j <- 0 until m) {
		for(i <- 0 until n) {
		}
	}
	// =================================================================
	// command processing
}

/* Temp Record to insert input */
class Record(n: Int) extends Module {
	val io = IO(new Bundle {
		val get = Input(UInt(8.W))
		val toCell = Output(UInt(8.W))
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
	io.toNext := r1
}
