package coverage.cone

case class Cone(statements: Seq[ConeStatement]) {
  lazy val withIndex = statements.zipWithIndex
  lazy val terminalIndexes = withIndex.collect {
    case (t: Terminal, idx) => idx
  }
  lazy val tag: String = {
    val hash = this.hashCode().toHexString
    "0" * (8 - hash.length) + hash
  }
  def serialize(indent: String = "") = {
    val idxWidth = statements.length.toString.size
    def serializeIndex(i: Int): String = {
      val idxStr = i.toString
      val nSpaces = idxWidth - idxStr.length
      idxStr + (" " * nSpaces)
    }
    s"${indent}Cone $tag:\n" + withIndex.map {
      case (Terminal(width, tpe), idx) =>
        s"${indent}  ${serializeIndex(idx)}: terminal $tpe<$width>"
      case (Literal(width, tpe, value), idx) =>
        s"${indent}  ${serializeIndex(idx)}: literal $tpe<$width>($value)"
      case (Invalid(width, tpe), idx) =>
        s"${indent}  ${serializeIndex(idx)}: invalid $tpe<$width>"
      case (Assign(op, args, consts), idx) =>
        s"${indent}  ${serializeIndex(idx)}: assign $op((${args.mkString(", ")}), (${consts.mkString(",")}))"
    }.mkString("\n")
  }

}



