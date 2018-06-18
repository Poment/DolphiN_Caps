package DolphiN

import chisel3._
import chisel3.util._

/* CORE CIRCUIT */
class RecordController(s: Int) extends Module {
	val io = IO(new Bundle {
		val begin = Input(Bool())
		val a = Input(UInt(8.W))
		val b = Input(UInt(8.W))
		val k = Input(UInt(8.W))
		val finish = Output(Bool())
	})
	// Register Setting ===================================================
	val timer = RegInit(255.U(16.W))
	val counter = RegInit(0.U(8.W))
	// Modules =========================================================
	// >>>> Queues setting
	val Mrecords = Vec(Seq.fill(s){ Module(new Record(s)).io })
	val Nrecords = Vec(Seq.fill(s){ Module(new Record(s)).io })
	// >>>> Elements setting
	val Elements = Vec(Seq.fill(s*s){ Module(new Element()).io })
	// >>>> Storage setting
	val TheStorage = Module(new Storage(s))
	// IO ==============================================================
	// >>>> Elements - Storages
	for(i <- 0 until s*s) {
		TheStorage.io.fromElements(i) := Elements(i).toAccumulator
	}
	// >>>> Element - Element
	// >> Left - Right
	for(j <- 0 until s) {
		for(i <- 0 until s-1) {
			Elements(i+1+(j*s)).fromLeft := Elements(i+(j*s)).toRight
		}
	}
	// >> Top - Down
	for(j <- 0 until s-1) {
		for(i <- 0 until s) {
			Elements(((j+1)*s)+i).fromTop := Elements((j*s)+i).toDown
		}
	}
	// >>>> Queue - Element
	for(i <- 0 until s) {
		Elements(i*s).fromLeft := Mrecords(i).toElement
	}
	for(i <- 0 until s) {
		 Elements(i).fromTop := Nrecords(i).toElement
	}
	// Controlling ========================================================
	// >>>> timer control & queue control
	// io.begin : true
	when( io.begin ) {
		timer := timer - 1.U
		Mrecords(counter).begin := true.B
		Nrecords(counter).begin := true.B
		counter := ((counter + 1.U) % s.U)
	}
	// io.begin : false
	.otherwise {
		timer := io.a + io.k + io.b - 1.U
		for(i <- 0 until s) {
			Mrecords(i).begin := false.B
			Nrecords(i).begin := false.B
		}
		counter := 0.U
	}
	// >>>> reset control
	TheStorage.io.reset := true.B
	io.finish := false.B
	// timer == 0
	when(timer === 0.U) {
		TheStorage.io.reset := true.B
		io.finish := true.B
		timer := 255.U
	}
	// timer != 0
	when(timer =/= 0.U) {
		TheStorage.io.reset := false.B
		io.finish := false.B
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
	val buffers = Reg(init = Vec(Seq.fill(x)(0.U(8.W))))
	// Controlling
	// io.get : true
	when( io.get ) {
		for (i <- x-1 to 1 by -1) {
    			buffers(i) := buffers(i-1)
  		}
  		buffers(0) := io.fromController
		io.toElement := 0.U
	}
	// io.get : false && io.begin : true
	.elsewhen( io.begin ) {
		for (i <- x-1 to 1 by -1) {
    			buffers(i) := buffers(i-1)
  		}
		io.toElement := buffers(x-1)
		buffers(0) := 0.U
	}
	// io.get : false && io.begin : false
	.otherwise {
		for (i <- 0 until x) {
    			buffers(i) := buffers(i)
			io.toElement := 0.U
  		}
	}
}

/* Matrix mul result Element */
/* Complete !!!!!            */
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
class Storage(x: Int) extends Module {
	val io = IO(new Bundle {
		val reset = Input(Bool())
		val fromElements = Input(Vec(x*x, UInt(8.W)))
		val toController = Output(UInt((32*x*x).W))
	})
	// Accumulators setting
	val accumulator = RegInit(Vec(Seq.fill(x*x)(0.U(32.W))))
	// match element and storage
	// io.reset : true -> reset
	when(io.reset) {
		for(i <- 0 until (x*x)) {
			accumulator(i) := 0.U
		}
	}
	// io.rest : false -> get from elements
	.otherwise {
		for(i <- 0 until (x*x)) {
			accumulator(i) := accumulator(i) + io.fromElements(i)
		}
	}
	// how concat
}
