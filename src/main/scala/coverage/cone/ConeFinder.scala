package coverage.cone

import firrtl.analyses._
import firrtl.annotations._
import firrtl.ir._
import firrtl.{FEMALE, MALE, PortKind}

import scala.collection.mutable


case class ConeFinder(reverseGraph: ConnectionGraph) extends ConnectionGraph(reverseGraph.circuit, reverseGraph.digraph, reverseGraph.irLookup) {
  private val extModuleNames = circuit.modules.collect { case e: ExtModule => e.name }.toSet
  private val ref2index = mutable.HashMap[ReferenceTarget, Int]()
  private val index2refs = mutable.HashMap[Int, Set[ReferenceTarget]]()
  private val stmts = mutable.HashMap[Int, ConeStatement]()
  private val endpoints = mutable.HashSet[ReferenceTarget]()


  /** Finds clock sources of specific signals
    *
    * If target is:
    *   - ReferenceTarget; calculate clock source, must be ground type
    *   - InstanceTarget; calculate clock source of all input ports to that instance
    *   - ModuleTarget; calculate clock source of all output ports
    *   - CircuitTarget; calculate clock source of all output ports of all modules
    *
    * @param targets
    * @return
    */
  /*
  def getClockSources(targets: Seq[CompleteTarget]): Map[ReferenceTarget, Set[ReferenceTarget]] = {
    val memberTargets: Seq[IsMember] = targets.map {
      case ct: CircuitTarget => ct.module(ct.circuit)
      case other: IsMember => other
    }.distinct

    val moduleOrder = new InstanceGraph(circuit).moduleOrder.zipWithIndex.map {
      case (m, i) => m.name -> i
    }.toMap

    val topoTargets = memberTargets.sortWith { (t0, t1) =>
      (t0, t1) match {
        case (x: CircuitTarget, _) => false
        case (_, x: CircuitTarget) => true
        case (x: IsMember, y: IsMember) => moduleOrder(x.module) > moduleOrder(y.module)
      }
    }

    val ret = memberTargets.foldLeft(Map.empty[ReferenceTarget, Set[ReferenceTarget]]) { (map, t) =>
      t match {
        case it: InstanceTarget =>
          val lit = it.asReference.pathlessTarget

          val inputTargets = lit.leafSubTargets(irLookup.tpe(lit)).collect {
            case r if irLookup.gender(r) == FEMALE => r
          }
          inputTargets.foldLeft(map){ (m, inTarget) =>
            val x = it.addReference(inTarget)
            m ++ Map(x -> getClockSource(x))
          }
        case rt: ReferenceTarget => map ++ Map(rt -> getClockSource(rt))
        case mt: ModuleTarget =>
          val outputTargets = irLookup.ports(mt).flatMap { irLookup.leafTargets }.collect {
            case r if irLookup.gender(r) == FEMALE => r
          }
          outputTargets.foldLeft(map) { (m, ot) => m ++ Map(ot -> getClockSource(ot)) }
      }
    }
    ret
  }
  */


  /** Returns the clock sources that are synchronized with given signal target
    * @param t
    * @return
    */
  def getCone(start: ReferenceTarget, ends: Seq[ReferenceTarget]): Cone = {
    // Check existence of targets
    // Check type of targets

    index2refs(0) = Set(start)
    ref2index(start) = 0
    endpoints ++= ends
    BFS(start)

    val cone = Cone(stmts.toSeq.sortBy { case (k, v) => k }.map(_._2))

    stmts.clear()
    index2refs.clear()
    ref2index.clear()
    endpoints.clear()

    cone
  }

  def getConeInfo(rt: ReferenceTarget): (BigInt, String) = {
    irLookup.tpe(rt) match {
      case UIntType(IntWidth(w)) => (w, "UInt")
      case SIntType(IntWidth(w)) => (w, "SInt")
      case ClockType => (BigInt(1), "Clock")
      case AsyncResetType => (BigInt(1), "AsyncReset")
      case other => sys.error(s"Illegal type: $other")
    }
  }

  override def getEdges(node: ReferenceTarget,
                        prevOpt: Option[collection.Map[ReferenceTarget, ReferenceTarget]]
                       ): collection.Set[ReferenceTarget] = {
    val prev = prevOpt.get

    assert(ref2index.contains(node), s"${node.serialize}")
    val index = ref2index(node)

    node match {
      // If find endpoint, tag path and end
      case rt if endpoints.contains(rt) =>
        val (width, tpe) = getConeInfo(rt)
        stmts(index) = Terminal(width, tpe)
        Set()

      // Top-level Input Port
      // Must check if not isClock because expression that is in the clock port of reg could be a port
      case rt@ ReferenceTarget(c, m, Nil, _, _)
        if irLookup.kind(rt) == PortKind && irLookup.gender(rt) == MALE && !rt.isClock =>
        val (width, tpe) = getConeInfo(rt)
        stmts(index) = Terminal(width, tpe)
        Set()
        //sys.error("Shouldn't reach a top-level input port! must be included in the endpoints.")

      // Black-box Output Clock Port
      case rt: ReferenceTarget
        if extModuleNames.contains(rt.encapsulatingModule) && irLookup.gender(rt) == FEMALE =>
        sys.error("Shouldn't reach a blackbox output port! must be included in the endpoints.")
        val (width, tpe) = getConeInfo(rt)
        stmts(index) = Terminal(width, tpe)
        Set()

      // WInvalid Expression
      case rt if ConnectionGraph.isInvalid(rt) =>
        val (width, tpe) = getConeInfo(rt)
        stmts(index) = Invalid(width, tpe)
        Set()

      // Literal Expression
      case rt if ConnectionGraph.isLiteral(rt) =>
        val int = rt.ref match {
          case TokenTagger.literalRegex(value) => value
        }
        val (width, tpe) = getConeInfo(rt)
        stmts(index) = Literal(width, tpe, BigInt(int))
        Set()

        /*
      case nonClockSource if hasShortCut(nonClockSource) =>
        val edges = super.getEdges(nonClockSource)
        val localSource = nonClockSource.pathlessTarget
        */


      case other =>
        val edges = super.getEdges(other)
        val exp = irLookup.expr(other)
        val indexes = edges.map{ rt =>
          index2refs(index2refs.keys.size) = Set(rt)
          val index = index2refs.keys.size - 1
          ref2index(rt) = index
          index
        }.toSeq
        exp match {
          case DoPrim(op, args, consts, tpe) =>
            stmts(index) = Assign(op.serialize, indexes, consts)
          case Mux(c, t, f, tpe) =>
            stmts(index) = Assign("mux", indexes, Nil)
          case ValidIf(c, v, tpe) =>
            stmts(index) = Assign("validif", indexes, Nil)
          case nonOp =>
        }
        edges
    }
  }
}
