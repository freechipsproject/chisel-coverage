package coverage

import mini._

class MiniCoverageSpec extends TileTests(
  SimpleTests,
  annotations = Seq(Coverages.testerALUCoverageAspect),
  params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
    case CacheBlockBytes => up(CacheBlockBytes)
    case Trace => false
  }})
)
