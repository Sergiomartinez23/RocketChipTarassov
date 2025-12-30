// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.
package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule
import freechips.rocketchip.util._

object QuantALUFunctions {
  val FN_QDW = 0.U
  val FN_QUP = 1.U
}

/**
 * QuantALU: ALU vectorial para activaciones cuantizadas.
 * Combina dos vectores de activaciones de 8 bits según una máscara de 2 bits.
 *  - 00: sin operación
 *  - 01: sin operación
 *  - 10: suma activación
 *  - 11: resta activación
 */
abstract class AbstractQuantALU(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {
    val in1 = Input(UInt(xLen.W)) // máscara
    val in2 = Input(UInt(xLen.W)) // activación 1
    val in3 = Input(UInt(xLen.W)) // activación 2
    val out = Output(UInt(xLen.W)) // salida
    val fn = Input(UInt(3.W))   // función (operación)
    val quant = Input(Bool())  // ancho de datos (no usado en esta versión)
  })
}

import QuantALUFunctions._

class QuantALU(implicit p: Parameters) extends AbstractQuantALU()(p) {
  override def desiredName = "RocketQuantALU"

  val mask = io.in1
  val activation1 = io.in2
  val activation2 = io.in3

  val elementsAct = 8     // 8 activaciones de 8 bits
  val elementsMask = 16   // 16 elementos de máscara de 2 bits

  // Desempaquetar la máscara (2 bits cada una)
  val maskVecUp = VecInit((0 until elementsMask).map(i => mask(2 * i + 33, 2 * i + 32)))
  val maskVecDown = VecInit((0 until elementsMask).map(i => mask(2 * i + 1, 2 * i)))
  val maskVec = Mux(io.fn === FN_QUP, maskVecUp, maskVecDown)

  // Desempaquetar activaciones (8 bits cada una)
  val activation1Vec = VecInit((0 until elementsAct).map(i => activation1(8 * i + 7, 8 * i)))
  val activation2Vec = VecInit((0 until elementsAct).map(i => activation2(8 * i + 7, 8 * i)))

  // Sumar ambas activaciones elemento a elemento
  val activationsVec = VecInit(activation1Vec ++ activation2Vec)

  val resultVec = VecInit((0 until elementsMask).map { i =>
    val wideAct = activationsVec(i).asSInt.pad(16)
    Mux(maskVec(i) === "b10".U,  wideAct,
    Mux(maskVec(i) === "b11".U, -wideAct, 0.S(16.W)))
  })

  when (io.quant) {
    for (i <- 0 until elementsMask) {
      printf("Activation: %d\n", activationsVec(i).asSInt)
      printf("Result: %d\n", resultVec(i))
    }
  }
  val result = resultVec.reduce(_ + _)

  io.out := Cat(Fill(48, result(15)), result(15,0)) 
  when (io.quant) {
   printf("Result pre io: %d\n", result)
   printf("Result: %d\n", io.out.asSInt) 
  }
}
