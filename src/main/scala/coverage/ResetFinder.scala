// See LICENSE for license details.

package firrtl.transforms.clockfinder

import firrtl.{FEMALE, MALE, MemKind, PortKind, RegKind}
import firrtl.analyses._
import firrtl.annotations.TargetToken.{Clock, Reset}
import firrtl.annotations._
import firrtl.ir._

import scala.collection.mutable


object ResetFinder {
  implicit class ResetFinderPML(circuitGraph: CircuitGraph) {
    def getResetSource(target: ReferenceTarget): Set[ReferenceTarget] = {
      val finder = new ResetFinder(circuitGraph.reverseConnectionGraph)
      finder.getResetSource(target)
    }
  }
}

/** Instance-Viewed Graph to find clock sources of signals */
class ResetFinder(reverseGraph: ConnectionGraph) extends ConnectionGraph(reverseGraph.circuit, reverseGraph.digraph, reverseGraph.irLookup) {

  /** Returns the clock sources that are synchronized with given signal target
    * @param t
    * @return
    */
  def getResetSource(t: ReferenceTarget): Set[ReferenceTarget] = {
    require(
      irLookup.contains(t),
      s"Cannot find\n${t.prettyPrint()}\nin circuit, when computing its clock source!"
    )

    val tpe = irLookup.tpe(t)

    val finalSources = TagSet()
    t.leafSubTargets(tpe).foreach { x =>
      BFS(x, Set.empty[ReferenceTarget])

      finalSources.update(resetMap.getTag(x).getOrElse(TagSet()))
    }
    finalSources.targets.toSet
  }

  private val extModuleNames = circuit.modules.collect { case e: ExtModule => e.name }.toSet

  // Maps signal to set of clock sources it is synchronized with
  private val resetMap = TagMap[ReferenceTarget, TagSet]()//mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]()

  // Utility function to determine if a target is a register
  private def isReg(t: ReferenceTarget): Boolean = {
    t.tryToComplete match {
      case rt: ReferenceTarget if !rt.isClock && !rt.isInit && !rt.isReset && irLookup.kind(t) == RegKind => true
      case other => false
    }
  }


  /** Returns instance-viewed combinational-edges or reg-to-clock edges
    * Ends early if visiting a node that was previously visited in another BFS
    * @param node the specified node
    * @param prevOpt
    * @return a Set[T] of all vertices that source has edges to
    */
  override def getEdges(node: ReferenceTarget,
                        prevOpt: Option[collection.Map[ReferenceTarget, ReferenceTarget]]
                       ): collection.Set[ReferenceTarget] = {
    val prev = prevOpt.get
    node match {
      // If cached result, record clock and end. Exclude cached top-level signals as input port could be a new result
      case rt if resetMap.getTag(rt).nonEmpty =>
        resetMap.tagPath(rt, prev, resetMap.getTag(rt).get)
        Set()

      // Top-level Input Port
      // Must check if not isClock because expression that is in the clock port of reg could be a port
      case rt@ ReferenceTarget(c, m, Nil, _, _)
        if irLookup.kind(rt) == PortKind && irLookup.gender(rt) == MALE && !rt.isClock =>
        //resetMap.tagPath(rt, prev, TagSet(Set(rt)))
        Set()

      // Black-box Output Clock Port
      case rt: ReferenceTarget
        if extModuleNames.contains(rt.encapsulatingModule) && irLookup.gender(rt) == FEMALE =>
        resetMap.tagPath(rt, prev, TagSet(Set(rt)))
        Set()

      // WInvalid Expression
      case rt if ConnectionGraph.isInvalid(rt) => Set()

      // Literal Expression
      case rt if ConnectionGraph.isLiteral(rt) => Set()

      case rt if irLookup.declaration(rt).isInstanceOf[DefRegister] =>
        val reset = super.getEdges(rt).collectFirst {
          case rt: ReferenceTarget if rt.tokens.last == Reset => super.getEdges(rt).head
        }.get
        resetMap.tagPath(rt, prev, TagSet(Set(reset)))
        Set()

      case nonClockSource => super.getEdges(nonClockSource)
    }
  }
}

