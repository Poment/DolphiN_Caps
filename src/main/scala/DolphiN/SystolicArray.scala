package DolphiN

import chisel3._
import chisel3.util._

/* CORE CIRCUIT */
class RecordController(m: Int, n: Int) extends Module {
	val io = IO(new Bundle {
		val begin = Input(Bool())
		val a = Input(UInt(8.W))
		val b = Input(UInt(8.W))
		val k = Input(UInt(8.W))
		val finish = Output(Bool())
	})
	// Register Setting ===================================================
	val timer = Reginit(2.U(16.W))
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
	// IO ==============================================================
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
			Elements(((j+1)*n)+i).fromTop := Elements((j*n)+i).toDown
		}
	}
	// >>>> Queue - Element
	for(i <- 0 until m) {
		Elements(i*n).fromLeft := Mrecords(i).toElement
	}
	for(i <- 0 until n) {
		 Elements(i).fromTop := Nrecords(i).toElement
	}
	// Controlling
	// >>>> timer control & Queue control
	when(io.begin) {
		timer := timer - 1.U
		Mrecords(counter).begin := true.B
		Nrecords(counter).begin := true.B
		counter := counter + 1.U
	}
	.otherwise {
		timer := io.a + io.k + io.b - 1.U
		for(i <- 0 until m) {
			Mrecords(i).begin := false.B
		}
		for(i <- 0 until n) {
			Nrecords(i).begin := false.B
		}
		counter := 0.U
	}
	// >>>> reset control
	TheStorage.io.reset := true.B
	when(timer === 0.U) {
		TheStorage.io.reset := true.B
		io.finish = true.B
		timer := 2.U
	}
	when(timer =/= 0.U) {
		TheStorage.io.reset := false.B
		io.finish = false.B
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
	// Controlling
	// >>>> when io.get is enable
	when( io.get ) {
		for (i <- x-1 to 1 by -1) {
    			buffers(i) := buffers(i - 1)
  		}
  		buffers(0) := io.fromController
		io.toElement := 0.U
  		
	}
	// >>>> when io.get is unable and io.begin is enable
	.elsewhen( io.begin ) {
			for (i <- x-1 to 1 by -1) {
    				buffers(i) := buffers(i - 1)
  			}
			io.toElement := buffers(x-1)
  		}
	}
	// >>>> when not get, not begin
	.otherwise {
		for (i <- 0 until x) {
    			buffers(i) := buffers(i)
			io.toElement := 0.U
  		}
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
		val reset = Input(Bool())
		val fromElements = Input(Vec( m * n, UInt(8.W) ))
		val toController = Output(UInt(( 32 * m * n ).W))
	})
	// accumulator
	val accumulator = RegInit(Vec(Seq.fill( m * n )( 0.U(32.W) )))
	val total = UInt(( 32 * m * n ).W)

	// match element and storage
	for(i <- 0 until (m * n)) {
		accumulator(i) := accumulator(i) + io.fromElements(i)
	}
	when(io.reset) {
		for(i <- 0 until (m * n)) {
			accumulator(i) := 0.U
		}
	}
	// Concat
	//for(i <- 0 until m * n) {
	//	total := Cat(accumulator(i), accumulator(i+1))
	//}
}
