package coverage

import coverage.cone.Cone
import firrtl.ir
import firrtl.annotations._


/**
  *
  * @param cone Logic cone that describes quanta
  * @param results maps bin category to coverage (if its been triggered) and name of bins matching category
  * @param mappings maps cone tip to cone endpoints
  */
case class Quanta(cone: Cone, results: Map[BinCategory, (Boolean, Set[String])], mappings: Map[ReferenceTarget, Seq[ReferenceTarget]]) {

  def tag: String = cone.tag

  private val defaults = results.getOrElse(Default, (false, Set.empty[String]))
  require(defaults._2.size <= 1, s"Cannot create quanta with more than one Default bin: $defaults")

  def update(newQuanta: Quanta): Quanta = {
    assert(newQuanta.cone == cone)
    val newResults = newQuanta.results.keysIterator.map { key =>
      results.get(key) match {
        case None => key -> newQuanta.results(key)
        case Some((false, names: Set[String])) =>
          val (newRes, newNames) = newQuanta.results(key)
          key -> (newRes, names.union(newNames))
        case Some((true, names)) =>
          val (_, newNames) = newQuanta.results(key)
          key -> (true, names.union(newNames))
      }
    }.toMap
    Quanta(cone, results ++ newResults, mappings ++ newQuanta.mappings)
  }

  def uncoveredBinCategories: Seq[BinCategory] = results.collect {
    case (k, (false, names)) => k
  }.toSeq

  def uncoveredIntervals: (Seq[(Int, Int)], Boolean) = {
    val bins = uncoveredBinCategories
    val default = uncoveredBinCategories.collectFirst { case Default => Default }
    val binIntervals = uncoveredBinCategories.filter { _.isInstanceOf[BinRange] }.sortBy {
      case BinRange(low, high) => low
    }
    val intervals = binIntervals.map{
      case BinRange(low, high) => (low.toInt, high.toInt)
    }
    (intervals, default.isDefined)
  }

}

object Quanta {
  def toMessage(signal: ReferenceTarget, cone: Cone, bins: Seq[Bin], database: Database): String = {
    val message = bins.zipWithIndex.foldLeft(s"${database.fullTag(cone)}!!${signal.serialize}") {
      case (str, (bin@Bin(label, category), idx)) =>
        category match {
          case BinRange(lo, hi) => str + s"!!$idx. $label $lo->$hi=%b"
          case Default          => str + s"!!$idx. $label default=%b"
        }
    }
    message
  }
  def fromMessages(database: Database, messages: Seq[String]): Database = {
    val ret = messages.foldLeft(database) { (db, string) =>
      val tokens = string.split("!!")
      val (index, tag) = tokens.head.split("#").toSeq match {
        case Seq(t: String, i: String) => (i.toInt, t)
      }
      val label = tokens(1)
      val rangedRegex = "([0-9]+)[.] (.+) ([0-9]+)->([0-9]+)=([01])".r
      val defaultRegex = "([0-9]+)[.] (.+) default=([01])".r
      val cone = db.tagMap(tag)(index)
      val quanta = db.quantas(cone)
      val results = tokens.drop(2).map {
        case rangedRegex(index, l, lo, hi, covered) =>
          BinRange(BigInt(lo), BigInt(hi)) -> (covered == "1", Set(l))
        case defaultRegex(index, l, covered) => Bin(label, Default)
          Default -> (covered == "1", Set(l))
      }.toMap[BinCategory, (Boolean, Set[String])]
      db.add(Quanta(cone, results, Map.empty))
    }
    ret
  }
}
