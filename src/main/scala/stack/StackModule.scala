package stack

import chisel3.stage.ChiselStage
import java.nio.file.Paths
import chisel3._
import chisel3.util._

// Your code starts here
class stack_memModule(dataWidth: Int, len: Int) extends Module {
  require(dataWidth > 0)
  require(len > 0)

  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(dataWidth.W))
    val underflow = Output(Bool())
    val overflow = Output(Bool())
    val isEmpty = Output(Bool())
    val isFull = Output(Bool())
    val popped = Output(Bool())
    val peeked = Output(Bool())
  })

  // --- State Elements ---
  val stack_mem = Reg(Vec(len, UInt(dataWidth.W)))
  val sp = RegInit(0.U(log2Ceil(len + 1).W))

  // Opcodes at bits [6:0]
  val push_op = "b0100111".U(7.W)
  val pop_op = "b1000011".U(7.W)
  val peek_op = "b1000000".U(7.W)

  val reg = Reg(UInt(dataWidth.W))

  reg := io.in

  // --- Input Decoding (Combinational) ---
  val opcode = io.in(6, 0)
  val payload = io.in(31, 7) // 25-bit immediate

  // Default outputs
  io.out := 0.U
  io.underflow := false.B
  io.overflow := false.B
  io.isEmpty := (sp === 0.U)
  io.isFull := (sp === len.U)
  io.popped := false.B
  io.peeked := false.B

  val empty = (sp === 0.U)
  val full = (sp === len.U)

  switch(opcode) {
    is(push_op) {
      when(full) { io.overflow := true.B }.otherwise {
        stack_mem(sp) := Cat(0.U(7.W), reg(dataWidth - 1, 7))
        sp := sp + 1.U
      }
    }

    is(pop_op) {
      when(empty) { io.underflow := true.B }.otherwise {
        io.out := stack_mem(sp - 1.U)
        io.popped := true.B
        sp := sp - 1.U
      }
    }

    is(peek_op) {
      when(empty) { io.underflow := true.B }.otherwise {
        io.out := stack_mem(sp - 1.U)
        io.peeked := true.B
      }
    }

  }
}

// Your code ends here

object SVGen extends App {
  val out = Paths
    .get(
      "out",
      this.getClass.getName
        .stripSuffix("$")
    )
    .toString
  new ChiselStage().emitSystemVerilog(
    new stack_memModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out)
  )
}
