package coverage

import chisel3._
import chisel3.aop.Select
import chisel3.aop.injecting.InjectingAspect
import firrtl.options.{RegisteredLibrary, ShellOption}
import mini.{ALU, Tile, TileTester}

object Coverages {
  def someBins: Seq[Bin] = Seq(
    Bin("Zero",    BinRange(0, 0)),
    Bin("One",  BinRange(1, 1)),
    Bin("Two",  BinRange(2, 2)),
    Bin("Three",  BinRange(3, 3)),
    Bin("Little",  BinRange(4, 100)),
    Bin("Big",     BinRange(101, 1000)),
    Bin("Huge",    BinRange(1001, 2000)),
    Bin("Default", Default))
  def everyBin(signal: Bits): Seq[Bin] = {
    val maxValue = math.pow(2.0, signal.getWidth.toDouble)
    val ticks = Range(0, maxValue.toInt, 1).toList
    ticks.map {
      case tick => Bin(tick.toString, BinRange(tick, tick))
    }
  }
  def ALUCoverage(alu: mini.ALU): Seq[CoverPoint] = {
    val endPoints = Seq(alu.io.A, alu.io.B, alu.io.alu_op)
    val covers = Seq(
      CoverPoint("alu", alu.io.out, endPoints, someBins),
      CoverPoint("alu", alu.io.sum, endPoints, someBins),
      CoverPoint("alu", alu.io.alu_op, endPoints, everyBin(alu.io.alu_op))
    )
    alu

    covers
  }

  def testerALUCoverage(tester: TileTester): Seq[CoverPoint] = {
    ALUCoverage(tester.dut.asInstanceOf[Tile].core.dpath.alu)
  }

  val testerALUCoverageAspect = CoverAspect(testerALUCoverage,
    Nil,
    CoverageOptions(Map(
      //DesignDone -> { tester: TileTester => tester.isDone },
      SimulatorDone -> { tester: TileTester => tester.setDone }
    ))
  )

  val testerALULogger = InjectingAspect({tester: TileTester => Seq(tester.dut.asInstanceOf[mini.Tile].core.dpath.alu)},{
    alu: ALU => {
      printf("reset == %b, out == %d, sum == %d\n", alu.reset.asUInt(), alu.io.out, alu.io.sum)
    }
  })
}

/*

case class MiniCoverage() extends RegisteredLibrary {
  val name = "Mini-Coverage"
  val options = Seq(new ShellOption[String](
    longOption = "floorplan",
    toAnnotationSeq = {
      case "dci" => Seq(FloorplanAspect("Mini_DCI","test_run_dir/html/myfloorplan",{ t: TileBase => Floorplans.layoutTile(t, 0) }))
      case "icd" => Seq(FloorplanAspect("Mini_ICD","test_run_dir/html/myfloorplan",{ t: TileBase => Floorplans.layoutTile(t, 1) }))
    },
    helpText = "The name of a mini floorplan must be <dci|icd> indicating the relative positions of the icache, core, and dcache.",
    helpValueName = Some("<dci|icd>")))
}
*/
