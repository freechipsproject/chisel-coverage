package coverage

import mini.{TagCapture, TaggedLineProcessor}
import coverage.cone.{Cone, ConeFinder}
import firrtl.analyses.ConnectionGraph
import firrtl.annotations.TargetToken.Index
import firrtl.annotations.{Annotation, ReferenceTarget, Target}
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, MALE, MidForm, RenameMap, ResolveAndCheck, ResolvedAnnotationPaths, Transform, WRef, WSubIndex}
import firrtl.ir._

case class CoveragePoint(start: ReferenceTarget,
                         ends: Seq[ReferenceTarget],
                         label: String,
                         bins: Seq[(ReferenceTarget, Bin)],
                         clock: ReferenceTarget,
                         printEnable: ReferenceTarget) extends Annotation {
  def toView(cone: Cone): View = {
    View(cone, start, bins.map(_._2), 1, 100)
  }

  def coverageModule: String = {
    val enclosingModules = (clock +: printEnable +: bins.map(_._1)).map(_.pathlessTarget.module).toSet
    assert(enclosingModules.size == 1, s"Cannot have bins, clock and printEnable not share the same module: $enclosingModules")
    enclosingModules.head
  }

  override def getTargets: Seq[Target] = (start +: ends) ++ bins.map(_._1) ++ Seq(clock, printEnable)

  override def update(renames: RenameMap): Seq[CoveragePoint] = {
    def rename(r: ReferenceTarget): ReferenceTarget = {
      renames.get(r).map(_.toSet.toSeq) match {
        case None => r
        case Some(Seq(n: ReferenceTarget)) => n
        case other => sys.error(s"Cannot propagate $this with 1:Many rename of $r: $other")
      }
    }

    Seq(CoveragePoint(
      rename(start),
      ends.map(rename),
      label,
      bins.map(x => (rename(x._1), x._2)),
      rename(clock),
      rename(printEnable)
    ))
  }
}

class CoverTransform extends Transform {
  override def inputForm: CircuitForm = MidForm

  override def outputForm: CircuitForm = MidForm

  //override val annotationClasses: Traversable[Class[_]] = List(classOf[CoveragePoint])

  def instrument(circuit: Circuit, coverages: Seq[CoveragePoint], database: Database): (Circuit, AnnotationSeq) = {
    val coneGraph = ConeFinder(ConnectionGraph(circuit).reverseConnectionGraph)

    val moduleMap = circuit.modules.map(m => m.name -> m).toMap

    val (newModuleMap, newAnnos, newDatabase, views) = coverages.foldLeft((moduleMap, Seq.empty[Annotation], database, Seq.empty[(String, View)])) {
      case ((moduleMapSoFar, annos, db, vs), cp@CoveragePoint(start, ends, label, bins, clock, printEnable)) =>
        val cone = coneGraph.getCone(start, ends)
        val results = bins.map { case (reg, Bin(l, category)) => category -> (false, Set(l)) }.toMap
        val quanta = Quanta(cone, results, Map(start -> ends))
        val trimmedQuanta = db.trim(quanta)
        val newDatabase = db.add(trimmedQuanta)
        val uncovered = trimmedQuanta.uncoveredBinCategories
        val uncoveredBins = bins.collect {
          case (rt, bin@Bin(l, b)) if uncovered.contains(b) => (rt, bin)
        }
        val removedBins = bins.collect {
          case (rt, bin@Bin(l, b)) if !uncovered.contains(b) => rt
        }
        val message = Quanta.toMessage(start, cone, uncoveredBins.map(_._2), newDatabase)
        val expsAll = uncoveredBins.foldLeft(Seq.empty[Expression]) {
          case (exps, (ReferenceTarget(_, _, _, ref, Seq(Index(idx))), _)) =>
            val exp = WSubIndex(WRef(ref), idx, UnknownType, MALE)
            exps :+ exp
        }
        val updatedModule = moduleMapSoFar(cp.coverageModule) mapStmt update(message, expsAll, removedBins, printEnable, clock)

        (moduleMapSoFar + (updatedModule.name -> updatedModule), TagCapture(newDatabase.fullTag(cone)) +: annos, newDatabase, (cp.label, cp.toView(cone)) +: vs)
    }
    newDatabase.writeCoverage()

    def processMessages(messages: Seq[String]): Unit = {
      /*
      val database = Database.readCoverage(newDatabase.name)
      val finalDatabase = Quanta.fromMessages(database, messages)
      finalDatabase.writeCoverage()
      */
      val db = Database.updateCoverage(newDatabase.name, messages, Quanta.fromMessages)
      val viewMap = views.groupBy{kv => kv._1}
      val groups = viewMap.map {
        case (label, views) => GroupView(label, 1, 100, views.map(_._2))
      }
      groups.foreach { g => println(g.serialize(db)) }
    }

    (circuit.copy(modules = circuit.modules.map(m => newModuleMap(m.name))),
      TaggedLineProcessor(processMessages) +: newAnnos)
  }

  def update(message: String,
             exps: Seq[Expression],
             removedBins: Seq[ReferenceTarget],
             printCoverage: ReferenceTarget,
             clockRef: ReferenceTarget
            )(body: Statement): Statement = {
    val removedNames = removedBins.map(_.ref).toSet

    def removeUnusedBinRegs(s: Statement): Statement = s match {
      case x: DefRegister if removedNames.contains(x.name) =>
        // Rely on DCE to remove
        //DefWire(x.info, x.name, x.tpe)
        x
      case other => other mapStmt removeUnusedBinRegs
    }

    def addPrintf(s: Statement): Statement = {
      val print = Print(NoInfo, StringLit(message + "\n"), exps, WRef(clockRef.ref), WRef(printCoverage.ref))
      Block(Seq(s, print))
    }

    addPrintf(removeUnusedBinRegs(body))
  }

  override def execute(state: CircuitState): CircuitState = {
    val coverages = state.annotations.collect {
      case c: CoveragePoint => c
    }


    val database = {
      val dbs = state.annotations.collect {
        case DatabaseAnnotation(dbName, true) => Database.populateCoverage(dbName)
        case DatabaseAnnotation(dbName, false) => Database.readCoverage(dbName)
      }.toSet
      assert(dbs.nonEmpty, s"Must have a database to use CoverTransform!")
      dbs.reduce(_.add(_))
    }

    val (newCircuit, newAnnotations) = instrument(state.circuit, coverages, database)


    val newState = state.copy(circuit = newCircuit, annotations = newAnnotations ++ state.annotations.filter {
      case c: CoveragePoint => false
      case c: DatabaseAnnotation => false
      case other => true
    })
    new ResolveAndCheck().execute(newState)
  }
}
