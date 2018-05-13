package DolphiN_Caps

import chisel3._
import chisel3.util._

/* CORE CIRCUIT */
class RecordController(m: Int, n: Int) extends Module {
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
	val TheStorage = Module(new Storage(m, n))
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
class Record(x: Int) extends Module {
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

/* Matrix mul result Element */
class Element extends Module {
	val io = IO(new Bundle {
		val fromLeft = Input(UInt(8.W))
		val fromTop = Input(UInt(8.W))
		val toRight = Output(UInt(8.W))
		val toDown = Output(UInt(8.W))
		val toBuffer = Output(UInt(16.W))
	})
	// from top / left to down / right
	val r0 = RegNext(io.fromLeft)
	val r1 = RegNext(r0)
	val r2 = RegNext(io.fromTop)
	val r3 = RegNext(r2)
	when(reset.toBool) {
		r0 := 0.U
		r1 := 0.U
		r2 := 0.U
		r3 := 0.U
	}
	// Multiply and send to Buffer
	val mul = r0 * r2
	io.toBuffer := mul
	// Systolic Array
	io.toRight := r1
	io.toDown := r3
}

/* Result storage */
class Storage(m: Int, n: Int) extends Module {
	val io = IO(new Bundle {
		// x storages
		val fromBuffers = Vec(Seq.fill( m * n ){ Input(UInt(16.W)) })
		val jobEnd = Input(Bool()) // if job is ended, send result
		val toInterface = Output(UInt(( 16 * m * n ).W)
		val catEnd = Output(bool())
	})
	// Receive and Concat
	val blocks = Reg(Vec(Seq.fill(m * n){ UInt(16.W) }))

	// match element and storage
	for(i <- 0 until m * n) {
		blocks(i) := io.fromBuffers(i)
	}

	val concat = UInit(0.U)
	when(io.jobEnd) {
		for(i <- 0 until (m * n)) {
			concat := Cat(concat, blocks(i)
		}
		io.toInterface := concat
		io.catEnd := TRUE
	}
	when(io.catEnd === TRUE) {
		io.catEnd := FALSE
	}
}
