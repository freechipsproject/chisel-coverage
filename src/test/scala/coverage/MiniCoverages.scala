package coverage

import chisel3.Bits
import chisel3.aop.Select
import firrtl.options.{RegisteredLibrary, ShellOption}
import mini.{Tile, TileTester}

object Coverages {
  def someBins: Seq[Bin] = Seq(Bin("Zero", BinRange(0, 0)), Bin("Little", BinRange(1, 100)), Bin("Big", BinRange(101, 1000)))
  def everyBin(signal: Bits): Seq[Bin] = {
    val maxValue = math.pow(2.0, signal.getWidth.toDouble)
    val ticks = Range(0, maxValue.toInt, 1).toList
    ticks.map {
      case tick => Bin(tick.toString, BinRange(tick, tick))
    }
  }
  def ALUCoverage(alu: mini.ALU): CoverGroup = {
    val points = Seq(
      CoverPoint("A", alu.io.A, someBins),
      CoverPoint("B", alu.io.B, someBins),
      CoverPoint("alu_op", alu.io.alu_op, everyBin(alu.io.alu_op)),
      CoverPoint("out", alu.io.out, someBins),
      CoverPoint("sum", alu.io.sum, someBins)
    )

    CoverGroup("ALU", alu, alu.clock, alu.reset, points)
  }

  def testerALUCoverage(tester: TileTester): Seq[CoverGroup] = {
    Seq(ALUCoverage(tester.dut.asInstanceOf[Tile].core.dpath.alu))
  }

  val testerALUCoverageAspect = CoverAspect(testerALUCoverage,
    CoverageOptions(Map(
      //DesignDone -> { tester: TileTester => tester.isDone },
      SimulatorDone -> { tester: TileTester => tester.setDone }
    ))
  )
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
