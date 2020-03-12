package coverage

import coverage.cone.{Assign, Cone, Terminal}
import firrtl.annotations.ReferenceTarget
import org.scalatest.FlatSpec
import firrtl.ir
import firrtl.ir.UIntType
import TestUtils._

object DatabaseSpec {
  case class IntWidth(width: BigInt) extends ir.Width {
    override def serialize: String = width.toString
  }

  val ref8 = ir.Reference("", UIntType(IntWidth(8)))
  val cone1 = Cone(Seq(Assign("add", Seq(1, 1), Nil), Terminal(8, "UInt")))
  val cone2 = Cone(Seq(Assign("sub", Seq(1, 1), Nil), Terminal(8, "UInt")))

  val lowFalse = BinRange(0, 10) -> (false, Set("low"))
  val lowTrue = BinRange(0, 10) -> (true, Set("low"))
  val defaultFalse = Default -> (false, Set.empty[String])
  val defaultTrue = Default -> (true, Set.empty[String])

  val quanta1 = Quanta(
    cone1,
    Map(lowFalse, defaultFalse),
    Map(("~Top|Top>add".RT, Seq("~Top|Top>x".RT, "~Top|Top>y".RT)))
  )
  val quanta2 = Quanta(
    cone2,
    Map(lowFalse, defaultTrue),
    Map(("~Top|Top>sub".RT, Seq("~Top|Top>x".RT, "~Top|Top>y".RT)))
  )
  val quanta3 = Quanta(
    cone2,
    Map(lowTrue, defaultTrue),
    Map(("~Top|Top>sub2".RT, Seq("~Top|Top>x".RT, "~Top|Top>z".RT)))
  )

  val db = Database(Map(quanta1.cone -> quanta1, quanta2.cone -> quanta2), "Smoke")

  def writeDB: Database = {
    db.writeCoverage("Smoke")
    db
  }
}

class DatabaseSpec extends FlatSpec {
  import DatabaseSpec._

  "Serialization and deserialization" should "work" in {
    val db = writeDB
    val readDB = Database.readCoverage("Smoke")
    assert(db == readDB)
  }

  "Updating quantas" should "work" in {
    val newDB = db.add(quanta3)
    val q3coverage = newDB.coverage(quanta3.cone)
    val q3mappings = newDB.targets(quanta3.cone)
    assert(q3coverage(lowFalse._1)._1)
    assert(q3coverage(defaultFalse._1)._1)
    assert(q3mappings.contains("~Top|Top>sub".RT))
    assert(q3mappings.contains("~Top|Top>sub2".RT))
  }

}
