package coverage

import mini._

case class TestBuilder(testSeq: String*) extends TestType {
  val tests = testSeq.toList
  val maxcycles = 15000L
}
class XORMiniCoverageSpec extends TileTests(
  TestBuilder("rv32ui-p-xor"),
  annotations = Seq(Coverages.testerALUCoverageAspect, DatabaseAnnotation("XORALUCoverage", true)),
  params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
    case CacheBlockBytes => up(CacheBlockBytes)
    case Trace => false
  }})
)

class SHLMiniCoverageSpec extends TileTests(
  TestBuilder("rv32ui-p-sll"),
  annotations = Seq(Coverages.testerALUCoverageAspect, DatabaseAnnotation("SHLALUCoverage", true)),
  params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
    case CacheBlockBytes => up(CacheBlockBytes)
    case Trace => false
  }})
)

class BothMiniCoverageSpec extends TileTests(
  TestBuilder("rv32ui-p-xor", "rv32ui-p-sll"),
  annotations = Seq(Coverages.testerALUCoverageAspect, DatabaseAnnotation("BothALUCoverage", true)),
  params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
    case CacheBlockBytes => up(CacheBlockBytes)
    case Trace => false
  }})
)

class ISAMiniCoverageSpec extends TileTests(
  ISATests,
  annotations = Seq(Coverages.testerALUCoverageAspect, DatabaseAnnotation("ISAALUCoverage", true)),
  params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
    case CacheBlockBytes => up(CacheBlockBytes)
    case Trace => false
  }})
)
