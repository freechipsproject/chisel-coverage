package coverage

import chisel3.{Bits, Bool, Data}
import chisel3.aop.injecting.{InjectingAspect, InjectingTransform}
import chisel3.aop.{Aspect, Select}
import chisel3.experimental.{ChiselAnnotation, RawModule, annotate}
import chisel3.util.experimental.BoringUtils
import firrtl.{AnnotationSeq, RenameMap}
import firrtl.annotations.{Annotation, IsMember}
import firrtl.options.Unserializable
import firrtl.passes.wiring.WiringTransform
import firrtl.stage.RunFirrtlTransformAnnotation

import scala.collection.{MapLike, mutable}
import scala.reflect.runtime.universe.TypeTag


trait CoverageOption

case object SimulatorDone extends CoverageOption

case class CoverageOptions(options: Map[CoverageOption, Any]) {
  def simDone[T <: RawModule](top: T): Bool = options(SimulatorDone).asInstanceOf[T => Bool](top)
}

case class CoverAspect[T <: RawModule](buildCoverage: T => Seq[CoverPoint],
                                       databases: Seq[String],
                                       coverage: CoverageOptions)
                                      (implicit tTag: TypeTag[T]) extends Aspect[T] {

  override def toAnnotation(top: T): AnnotationSeq = {
    val coverpoints = buildCoverage(top)
    val pointMap = coverpoints.groupBy { point => point.signal.toTarget.module }
    val annoSeqs = Select.collectDeep(top) {
      case x: RawModule if pointMap.contains(x.toTarget.module) =>
        val points = pointMap(x.toTarget.module).toList
        val firrtlAnnotations = mutable.ArrayBuffer[CoveragePoint]()
        InjectingAspect[T, RawModule](
          (t: T) => Seq(x),
          { m: RawModule =>
            import chisel3._
            val done = Wire(Bool())
            done := DontCare
            BoringUtils.bore(coverage.simDone(top), Seq(done))
            points.foreach { cp =>
              val tracker = Module(new CoverageTracker(cp))
              tracker.in := cp.signal
              tracker.printCoverage := done
              firrtlAnnotations += tracker.collectCoveragePoint()
            }
          }
        ).toAnnotation(top) ++ firrtlAnnotations
    }
    val ret = annoSeqs.toList.foldRight(Seq[Annotation](RunFirrtlTransformAnnotation(new WiringTransform))) { case (sofar, next) => next ++ sofar }
    (RunFirrtlTransformAnnotation(new CoverTransform()) +: databases.map(DatabaseAnnotation(_, true))) ++ ret
  }
}
