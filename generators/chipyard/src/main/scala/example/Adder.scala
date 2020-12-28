package chipyard.example
import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, BaseModule}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

case class AdderParams(
                      address: BigInt = 0x2000,
                      width: Int = 32,
                      useAXI4: Boolean = false,
                      useBlackBox: Boolean = false)

case object AdderKey extends Field[Option[AdderParams]](None)

class AdderIO(val w: Int) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val input_ready = Output(Bool())  // indicating that adder is ready to accept data
  val input_valid = Input(Bool())   // indicating that the sender is sending valid data
  val x = Input(UInt(w.W))          // first operand to be added
  val y = Input(UInt(w.W))          // second operand to be added
  val output_ready = Input(Bool())  // inidicating that the sender is ready to accept the data
  val output_valid = Output(Bool()) // indicating that the adder is sending a valid data
  val sum = Output(UInt(w.W))
  val busy = Output(Bool())
}

trait HasAdderIO extends BaseModule {
  val w: Int
  val io = IO(new AdderIO(w))
}

class AdderChiselModule(val w: Int) extends Module
  with HasAdderIO
{
  val s_idle :: s_run :: s_done :: Nil = Enum(3)

  val state = RegInit(s_idle)
  val sum = Reg(UInt(w.W))
  val tmp = Reg(UInt(w.W))

  io.input_ready := state === s_idle
  io.output_valid := state === s_done
  io.sum := sum

  when(state === s_idle && io.input_valid) {
    state := s_run
  } .elsewhen(state === s_run) {
    state := s_done
  } .elsewhen(state === s_done && io.output_ready) {
    state := s_idle
  }

  when(state === s_idle && io.input_valid) {
    sum := io.x
    tmp := io.y
  } .elsewhen(state === s_run) {
    sum := sum + tmp
  }

  io.busy := state =/= s_idle
}

trait AdderModule extends HasRegMap {
  implicit val p: Parameters
  def params: AdderParams
  val clock: Clock
  val reset: Reset

  val x = Reg(UInt(params.width.W))
  val y = Wire(new DecoupledIO(UInt(params.width.W)))
  val sum = Wire(new DecoupledIO(UInt(params.width.W)))
  val status = Wire(UInt(2.W))

  val impl = Module(new AdderChiselModule(params.width))
  impl.io.clock := clock
  impl.io.reset := reset.asBool

  impl.io.x := x
  impl.io.y := y.bits
  impl.io.input_valid := y.valid
  y.ready := impl.io.input_ready

  sum.bits := impl.io.sum
  sum.valid := impl.io.output_valid
  impl.io.output_ready := sum.ready

  status := Cat(impl.io.input_ready, impl.io.output_valid)

  regmap(
    0x00 -> Seq(
      RegField.r(2, status)),   // a read-only register capturing current status
    0x04 -> Seq(
      RegField.w(params.width, x)), // a write only register that sets the first operand of the adder
    0x08 -> Seq(
      RegField.w(params.width, y)), // a write only register, sets the y.valid signal when written
    0x0C -> Seq(
      RegField.r(params.width, sum)))   // a read-only register, sets the sum.ready signal when read
}

class AdderTL(params: AdderParams, beatBytes: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    params.address, "adder", Seq("hadir,adder"),
    beatBytes = beatBytes)(
    new TLRegBundle(params, _))(
    new TLRegModule(params, _, _) with AdderModule)

trait CanHavePeripheryAdder { this: BaseSubsystem =>
  private val portName = "adder"
  val adder = p(AdderKey) match {
    case Some(params) => {
      if (!params.useAXI4) {
        val adder = LazyModule(new AdderTL(params, pbus.beatBytes)(p))
        pbus.toVariableWidthSlave(Some(portName)) { adder.node }
        Some(adder)
      }
    }
    case None => None
  }
}

class WithAdder(useAXI4: Boolean, useBlackBox: Boolean) extends Config((site, here, up) => {
  case AdderKey => Some(AdderParams(useAXI4 = useAXI4, useBlackBox = useBlackBox))
})
