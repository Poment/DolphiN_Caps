package circuit.matrix

import chisel3._
import chisel3.util._

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
class Storage extends Module {
	val io = IO(new Bundle {
		// m * n storages
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
		io.catEnd := FALSE
	}
}
