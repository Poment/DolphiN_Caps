package DolphiN

import chisel3._
import chisel3.util._

/* CORE CIRCUIT */
class RecordController(m: Int, n: Int) extends Module {
	val io = IO(new Bundle {
		val begin = Input(Bool())
	})
	// Modules =========================================================
	// >>>> Queue setting
	val Mrecords =
		Vec(Seq.fill(m){ Module(new Record(m+n)).io })
	val Nrecords =
		Vec(Seq.fill(n){ Module(new Record(n+m)).io })
	val counter = RegInit(0.U(8.W))
	// >>>> Element setting
	val Elements = Vec(Seq.fill(m * n){ Module(new Element()).io })
	// >>>> Accumulator setting
	val TheStorage = Module(new Storage(m, n))
	// Wires ===========================================================
	// >>>> Elements - Storages
	for(i <- 0 until m * n) {
		TheStorage.io.fromElements(i) := Elements(i).toAccumulator
	}

	// >>>> Element - Element
	// >> Left - Right
	for(j <- 0 until m) {
		for(i <- 0 until n-1) {
			Elements(i+1+(j*n)).fromLeft := Elements(i+(j*n)).toRight
		}
	}
	// >> Top - Down
	for(j <- 0 until m-1) {
		for(i <- 0 until n) {
			Elements(i+n).fromTop := Elements(i).toDown
		}
	}
	// >>>> Queue - Element
	for(i <- 0 until m) {
		Mrecords(i).toElement := Elements(i*n).fromLeft
	}
	for(i <- 0 until n) {
		Nrecords(i).toElement := Elements(i).fromTop
	}
	// Begin Queue
	when( io.begin ) {
		Mrecords(counter).begin := true
		Nrecords(counter).begin := true
		counter := counter + 1
	}
}

/* Temp Record to insert input */
class Record(x: Int) extends Module {
	val io = IO(new Bundle {
		val fromController = Input(UInt(8.W))
		val get = Input(Bool())
		val begin = Input(Bool())
		val toElement = Output(UInt(8.W))
	})
	// Initialize
	val buffers =  Reg(init = Vec(Seq.fill( x )( 0.U(8.W) )))
	// Get input
	when( io.get ) {
		for (i <- x-1 to 1 by -1) {
    			buffers(i) := buffers(i - 1)
  		}
  		buffers(0) := io.fromController
  		
	}
	// Give input
	when( io.begin ) {
		for (i <- x-1 to 1 by -1) {
    			buffers(i) := buffers(i - 1)
  		}
		io.toElement := buffers(x-1)
	}
}

/* Matrix mul result Element */
class Element extends Module {
	val io = IO(new Bundle {
		val fromLeft = Input(UInt(8.W))
		val fromTop = Input(UInt(8.W))
		val toRight = Output(UInt(8.W))
		val toDown = Output(UInt(8.W))
		val toAccumulator = Output(UInt(32.W))
	})
	// from top / left to down / right
	val r0 = RegInit(0.U(8.W))
	val r1 = RegInit(0.U(8.W))
	// Multiply and send to Buffer
	val mul = r0 * r1
	io.toAccumulator := mul
	// Systolic Array
	r0 := io.fromLeft
	r1 := io.fromTop
	io.toAccumulator := mul
	io.toRight := r0
	io.toDown := r1
}

/* Result storage */
class Storage(m: Int, n: Int) extends Module {
	val io = IO(new Bundle {
		// x storages
		val fromElements = Input(Vec( m * n, UInt(8.W) ))
		val finish = Input(Bool()) // if job is ended, send result
		val toController = Output(UInt(( 32 * m * n ).W))
	})
	// accumulator
	val accumulator = Reg(init = Vec(Seq.fill( m * n )( 0.U(32.W) )))

	// match element and storage
	for(i <- 0 until m * n) {
		accumulator(i) := io.fromElements(i)
	}
}
