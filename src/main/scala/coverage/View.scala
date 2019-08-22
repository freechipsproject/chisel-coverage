package coverage

import coverage.cone.Cone
import firrtl.annotations.ReferenceTarget

import scala.collection.mutable


case class GroupView(label: String, weight: Double, goal: Double, views: Seq[View]) {
  def uncovered(database: Database): Int = {
    views.count(v => v.percentage(database) < v.goal)
  }
  def covered(database: Database): Int = {
    views.count(v => v.percentage(database) >= v.goal)
  }
  def percentage(database: Database): Double = {
    covered(database).toDouble * 100 / views.length
  }
  def serialize(database: Database): String = {
    val group =
      s""" ===========================================================
         | Group : $label
         | ===========================================================
         | SCORE  WEIGHT GOAL
         | ${score(database)} $weight      $goal
         | -----------------------------------------------------------
         | Summary for Group   $label
         | CATEGORY  EXPECTED UNCOVERED COVERED PERCENT
         | Signals   ${views.size}        ${uncovered(database)}         ${covered(database)}       ${percentage(database)}
         | ===========================================================
         |""".stripMargin

    val signals =
      s"""| ===========================================================
          | Signals for Group  $label
          |""".stripMargin ++
        views.sortBy{v => v.start.serialize}
          .map(_.serialize(database))
          .mkString("\n")
    group + signals + " ==========================================================="
  }
  def score(database: Database): Double = {
    val allWeights = views.map(_.weight).sum
    val perViewScore = views.map(v => 1.0.min(v.percentage(database) / v.goal) * v.weight)
    perViewScore.sum * 100 / allWeights
  }
}
case class View(cone: Cone, start: ReferenceTarget, bins: Seq[Bin], weight: Double, goal: Double) {
  def coverage(database: Database): Seq[(Bin, Boolean)] = {
    val quanta = database.quantas(cone)
    bins.map { bin =>
      (bin, quanta.results(bin.category)._1)
    }
  }

  def uncovered(database: Database): Int = {
    coverage(database).count(!_._2)
  }

  def covered(database: Database): Int = {
    coverage(database).count(_._2)
  }
  def percentage(database: Database): Double = {
    covered(database).toDouble * 100 / bins.length
  }

  def endPoints(database: Database): Seq[ReferenceTarget] = database.quantas(cone).mappings(start)

  def serialize(database: Database): String = {
    val x = Seq(
      Seq(" SIGNAL", "EXPECTED", "UNCOVERED", "COVERED", "PERCENT", "GOAL", "WEIGHT"),
      Seq(" " + start.serialize, bins.size, uncovered(database), covered(database), percentage(database), goal, weight).map(_.toString)
    )
    val summary = " -----------------------------------------------------------\n" + alignRows(x)
    /*
    s"""
       | SIGNAL EXPECTED UNCOVERED COVERED PERCENT GOAL WEIGHT
       | $start          $expected         $uncovs       $per  $goal  $weight
       | Summary for Signal $start
       |
       | CATEGORY          EXPECTED UNCOVERED COVERED PERCENT
       | User Defined Bins $expected        $uncovs         $cov       $per
       |
       | User Defined Bins for $start
       | Bins
       |
       |"""
       */

    summary + "\n" + serializeBins(database)
  }
  def getWidth(seq: Seq[String]): Int = {
    seq.map(_.length()).max
  }
  def setWidth(seq: Seq[String]): Seq[String] = {
    val width = getWidth(seq)
    seq.map(s =>
      s + (" " * (width - s.length()))
    )
  }

  def alignRows(rows: Seq[Seq[String]]): String = {
    val ncols = rows.map(_.length).max
    val cols = mutable.ArrayBuffer[Seq[String]]()
    for(c <- 0 until ncols) {
      cols += rows.map(row => row(c))
    }
    alignCols(cols)
  }

  def alignCols(columns: Seq[Seq[String]]): String = {

    val alignedColWidths = columns.map(setWidth)

    val nrows = columns.map(_.length).max
    val ncols = columns.length
    val stringBuilder = new StringBuilder()
    for(r <- 0 until nrows) {
      for(c <-0 until ncols) {
        stringBuilder ++= (alignedColWidths(c)(r) + " ")
      }
      stringBuilder ++= "\n"
    }
    stringBuilder.toString
  }

  def serializeBins(database: Database): String = {
    val rep = coverage(database)
    val (ns, cs, cvs) = rep.foldLeft(Vector(" NAME"), Vector("CATEGORY"), Vector("COVERED")) {
      case ((names, categories, covereds), (bin, covered)) =>
        (names :+ (" " + bin.label), categories :+ bin.category.serialize, covereds :+ covered.toString)
    }
    alignCols(Seq(ns, cs, cvs))
  }
}
