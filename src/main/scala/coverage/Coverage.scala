package coverage

import firrtl.analyses.{CircuitGraph, ConnectionGraph}
import firrtl.annotations.{ModuleTarget, ReferenceTarget}
import firrtl.ir.{Circuit, Expression}
import firrtl.transforms.clockfinder.ClockFinder

/*
object Coverage {
  def computeCone(signal: ReferenceTarget, endPoint: ModuleTarget, circuit: Circuit): Expression = ???
  def computeCone(signal: ReferenceTarget, endPoints: Seq[ReferenceTarget], circuit: Circuit): Expression = {
    val cg = CircuitGraph(circuit)
    val pathscg.path(signal, endPoints)
    val connectionGraph = ConnectionGraph(circuit)
    val clockFinder = ClockFinder.ClockFinderPML()

  }

}
*/
