package coverage

import firrtl.annotations.ReferenceTarget

object TestUtils {
  implicit class StringRefTypeClass(str: String) {
    def RT: ReferenceTarget = {
      firrtl.annotations.Target.deserialize(str) match {
        case r: ReferenceTarget => r
        case other => sys.error(s"Cannot convert $str to a ReferenceTarget: $other")
      }
    }
  }

}
