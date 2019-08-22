package coverage

import chisel3.Bits
import firrtl.RenameMap
import firrtl.annotations.{Annotation, IsMember}
import firrtl.options.Unserializable


object SignalTracker {
  def apply(signal: Bits, selector: Seq[IsMember] => Option[IsMember] = ts => Some(ts.head)): SignalTracker = {
    SignalTracker(signal, Nil, selector, expanded = false)
  }
}

case class SignalTracker(signal: Bits, targets: Seq[IsMember], finalSelection: Seq[IsMember] => Option[IsMember], expanded: Boolean) extends Annotation with Unserializable {
  def singleUpdate(renames: RenameMap): SignalTracker = {
    val renamed = update(renames)
    assert(renamed.size == 1, "Signal Tracker should always be renamed to a single other SignalTracker")
    renamed.head
  }
  Seq(1, 2).distinct

  override def update(renames: RenameMap): Seq[SignalTracker] = {
    val expandedTargets = if(!expanded) {
      assert(targets.isEmpty, "If SignalTracker isn't expanded, its targets should be empty.")
      Seq(signal.toTarget)
    } else targets
    val newMembers = expandedTargets.flatMap { m: IsMember =>
      renames.get(m) match {
        case Some(seq) => seq
        case None => Seq(m)
      }
    }
    if(!expanded) Seq(this.copy(targets = newMembers, expanded=true)) else Seq(this.copy(targets = newMembers))
  }
}

