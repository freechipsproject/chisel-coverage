package coverage.cone

sealed trait ConeStatement
case class Terminal(width: BigInt, tpe: String) extends ConeStatement
case class Literal(width: BigInt, tpe: String, value: BigInt) extends ConeStatement
case class Invalid(width: BigInt, tpe: String) extends ConeStatement
case class Assign(op: String, args: Seq[Int], consts: Seq[BigInt]) extends ConeStatement
case class ConeTerminal(cone: String, name: String) extends ConeStatement
case class ConeStart(cone: String, name: String) extends ConeStatement
case class ConeCopy(ends: Seq[String], start: String, cone: String) extends ConeStatement


