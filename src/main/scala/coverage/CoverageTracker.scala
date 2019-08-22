package coverage

import chisel3._
import chisel3.experimental.MultiIOModule
import firrtl.annotations.ReferenceTarget

/** Either records and bins the value of [in] every cycle, or cycles through all bins and prints the result
  *
  * @param point contains signal (and its type) as well as other histogramming information
  */
class CoverageTracker(point: CoverPoint) extends MultiIOModule {
  val in = IO(Input(chiselTypeOf(point.signal)))
  val printCoverage = IO(Input(Bool()))

  val intervals = point.intervals
  val default = point.default

  val lows = VecInit(intervals.map(_._2.U))
  val highs = VecInit(intervals.map(_._3.U))

  // Calculate in's address into histogram
  val defaultIndex = if(default.isDefined) intervals.length else 0

  val activeBinAddress = point.signal match {
    case _: UInt =>
      val inU = in.asUInt()
      intervals.zipWithIndex.foldLeft(defaultIndex.U) {
        case (addr: UInt, ((_, min: Int, max: Int), index: Int)) => Mux((inU >= min.U) & (inU <= max.U), index.U, addr)
      }
    case _: SInt =>
      val inS = in.asSInt()
      intervals.zipWithIndex.foldLeft(defaultIndex.U) {
        case (addr: UInt, ((_, min: Int, max: Int), index: Int)) => Mux((inS >= min.S) & (inS <= max.S), index.U, addr)
      }
  }

  // Records which bins have been written to (and which require initialization)
  val coverage = RegInit(VecInit(Seq.fill(math.pow(2, activeBinAddress.getWidth).toInt)(false.B)))

  // Then, do stuff
  when(reset.asBool() === false.B) {
    coverage(activeBinAddress) := true.B
    //printf(s"${point.signal.toTarget.serialize} is %d, bin %d\n", in, activeBinAddress)
  }

  def collectCoveragePoint(): CoveragePoint = {

    val bins = coverage.map(_.toTarget).zip(intervals.map(_._1) ++ default)
    CoveragePoint(point.signal.toAbsoluteTarget, point.endPoints.map(_.toAbsoluteTarget), point.label, bins, clock.toTarget, printCoverage.toTarget)
  }
}

/*

covergroup address_cov (ref logic [7:0] address,
  22        input int low, int high) @ (posedge ce);
  23     ADDRESS : coverpoint address {
  24       bins low    = {0,low};
  25       bins med    = {low,high};
  26     }
  27   endgroup
  28   //=================================================
  29   // Instance of covergroup
  30   //=================================================
  31   address_cov acov_low  = new(addr,0,10);
  32   address_cov acov_med  = new(addr,11,20);
  33   address_cov acov_high = new(addr,21,30);

 ===========================================================
 Group : coverage_covergroup.miff::address_cov
 ===========================================================
 SCORE  WEIGHT GOAL
 100.00 1      100
 -----------------------------------------------------------
 Summary for Group   coverage_covergroup.miff::address_cov
 CATEGORY  EXPECTED UNCOVERED COVERED PERCENT
 Variables 2        0         2       100.00

 Variables for Group  coverage_covergroup.miff::address_cov

 VARIABLE EXPECTED UNCOVERED COVERED PERCENT GOAL WEIGHT
 ADDRESS  2        0         2       100.00  100  1
 -----------------------------------------------------------
 Summary for Variable ADDRESS

 CATEGORY          EXPECTED UNCOVERED COVERED PERCENT
 User Defined Bins 2        0         2       100.00

 User Defined Bins for ADDRESS
 Bins

 NAME COUNT AT LEAST
 med  2     1
 low  6     1
 */

/*
covergroup datac @ (negedge cif.cb.ce);
  83      data_in : coverpoint cif.cb.datai {
  84        bins low    = {0,50};
  85        bins med    = {51,150};
  86        bins high   = {151,255};
  87      }
  88      data_out : coverpoint cif.cb.datao {
  89        bins low    = {0,50};
  90        bins med    = {51,150};
  91        bins high   = {151,255};
  92      }
  93      read_write : coverpoint cif.cb.we {
  94        bins  read  = {0};
  95        bins  write = {1};
  96      }
  97   endgroup

 VARIABLE EXPECTED UNCOVERED COVERED PERCENT GOAL WEIGHT
 address  3        2         1       33.33   100  1

 VARIABLE   EXPECTED UNCOVERED COVERED PERCENT GOAL WEIGHT
 data_in    3        2         1       33.33   100  1
 data_out   3        3         0       0.00    100  1
 read_write 2        0         2       100.00  100  1
 */