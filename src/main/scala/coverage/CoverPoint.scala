package coverage

import chisel3._
import chisel3.experimental.ChiselAnnotation
import firrtl.RenameMap
import firrtl.annotations.{Annotation, NoTargetAnnotation, Target}

//trait CoverBase { val label: String }


/*
object CoverPoint {
  def apply(label: String, signal: Bits, endPoints: Seq[Bits], clock: Clock, reset: Reset, bins: Seq[Bin], pointOptions: CoverOptions = CoverOptions()): CoverPoint = {
    CoverPoint(label, SignalTracker(signal), endPoints.map(SignalTracker(_)), clock, reset, bins, pointOptions)
  }
}
*/
case class CoverPoint (label: String,
                       signal: Bits,
                       endPoints: Seq[Bits],
                       bins: Seq[Bin]) {
  def default: Option[Bin] = {
    bins.collectFirst { case b@Bin(l, Default) => b }
  }
  def intervals: Seq[(Bin, Int, Int)] = {
    val binIntervals = bins.filter { _.category.isInstanceOf[BinRange] }.sortBy {
      case Bin(l, BinRange(low, high)) => low
    }
    val intervals = binIntervals.map{
      case b@Bin(l, BinRange(low, high)) => (b, low.toInt, high.toInt)
    }
    intervals
  }
}

case class CoverOptions(weights: Seq[Int] = Seq(1), maxCount: Int = 32)

// TODO: I think you can get away without representing this directly and generating it programmatically
// case class CrossPoint(name: String, points: Seq[CoverPoint], bins: Seq[BaseBin]) extends CoverBase

abstract class BaseBin {
  val labelOption: Option[String]
  val category: BinCategory
}

// Explicit bin, bins based on category
case class Bin(label: String, category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = Some(label)
}

// Implicit bin, bins based on category
// Not user created
case class ImplicitBin(category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = None
}

// Ignores when bin matches (usually paired with ImplicitBin
case class IgnoreBin(label: String, category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = Some(label)
}


trait BinCategory {
  def serialize: String
}

// Defaults to all non-specified categories
case object Default extends BinCategory {
  override def serialize: String = "default"
}

// Low and High are inclusive
case class BinRange(low: BigInt, high: BigInt) extends BinCategory {
  override def serialize: String = s"$low->$high"
}

// A sequence of values that must be transitioned to, in order
// Wait on this...
//case class BinTransition(sequence: Seq[BinValue]) extends BinCategory

// Unnecessary!
//trait BinValue

// A value in a sequence that must match immediately
//case class BinConstant(value: BigInt) extends BinValue

// A value that must be hit eventually, but not necessarily at this time
//case class BinEventually(value: BigInt) extends BinValue


