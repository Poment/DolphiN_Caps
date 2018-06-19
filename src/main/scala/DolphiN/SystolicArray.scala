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
		val result = Output(SInt((32*s*s).W))
	})
	// Register Setting ================================================
	val timer = RegInit(255.U(16.W))
	val counter = RegInit(0.U(8.W))
	// Modules =========================================================
	// >>>> Elements setting
	val Elements = Vec(Seq.fill(s*s){ Module(new Element()).io })
	// >>>> Storage setting 
	val TheStorage = Module(new Storage(s))
	// >>>> Queue setting
	val MQ = Vec(Seq.fill(s){ Module(new Queue(SInt(8.W),s*2)).io })
	val NQ = Vec(Seq.fill(s){ Module(new Queue(SInt(8.W),s*2)).io })
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
	// Controlling ========================================================
	// >>>> timer control & queue control
	// io.begin : true
	when( io.begin ) {
		timer := timer - 1.U
		for(i <- 0 until s) {
			Elements(i).fromTop <> NQ(i).deq
		}
		for(i <- 0 until s) {
			Elements(i*s).fromLeft <> MQ(i).deq
		}
	}
	// io.begin : false
	.otherwise {
		timer := io.a + io.k + io.b - 1.U
		for(i <- 0 until s) {
			Elements(i).fromTop := 0.S
		}
		for(i <- 0 until s) {
			Elements(i*s).fromLeft := 0.S
		}
	}
	// >>>> reset control
	TheStorage.io.reset := true.B
	//io.finish := false.B
	// timer == 0
	when(timer === 0.U) {
		TheStorage.io.reset := true.B
		timer := 255.U
		io.finish := true.B
	}
	// timer != 0
	.elsewhen(timer =/= 0.U) {
		TheStorage.io.reset := false.B
		io.finish := false.B
	}
	// result
	io.result := TheStorage.io.toController
}

/* Matrix mul result Element */
/* Complete !!!!!            */
class Element extends Module {
	val io = IO(new Bundle {
		val fromLeft = Input(SInt(8.W))
		val fromTop = Input(SInt(8.W))
		val toRight = Output(SInt(8.W))
		val toDown = Output(SInt(8.W))
		val toAccumulator = Output(SInt(32.W))
	})
	// from top / left to down / right
	val r0 = RegInit(0.S(8.W))
	val r1 = RegInit(0.S(8.W))
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
		val fromElements = Input(Vec(x*x, SInt(8.W)))
		val toController = Output(SInt((32*x*x).W))
	})
	// Accumulators setting
	val accumulator = RegInit(Vec(Seq.fill(x*x)(0.S(32.W))))
	val total = RegInit(0.S((32*x*x).W))
	// match element and storage
	// io.reset : true -> reset
	when(io.reset) {
		for(i <- 0 until (x*x)) {
			accumulator(i) := 0.S
		}
	}
	// io.rest : false -> get from elements
	.otherwise {
		for(i <- 0 until (x*x)) {
			accumulator(i) := accumulator(i) + io.fromElements(i)
		}
	}
	// how concat
	for(i <- 0 until (x*x)) {
		total(i*32,(i+1)*32) := accumulator(i)
	}
	io.toController := total
}
