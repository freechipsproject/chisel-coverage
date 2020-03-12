package coverage

import java.io._

import coverage.cone.Cone
import firrtl.annotations.{NoTargetAnnotation, ReferenceTarget}
import firrtl.ir

case class DatabaseAnnotation(name: String, createIfNew: Boolean) extends NoTargetAnnotation

case class Database(quantas: Map[Cone, Quanta], name: String) {
  lazy val tagMap: Map[String, Seq[Cone]] = quantas.keysIterator.toSeq.groupBy(cone => cone.tag)

  def add(other: Database): Database = {
    other.quantas.foldLeft(this) { case (db, (e, q)) => db.add(q) }
  }
  def add(quantas: Iterable[Quanta]): Database = {
    quantas.foldLeft(this){ (db, q) => db.add(q) }
  }
  def add(quanta: Quanta): Database = {
    val newQuanta = quantas.get(quanta.cone) match {
      case Some(existing) => existing.update(quanta)
      case None =>
        quanta
    }
    val ret = Database(quantas + (newQuanta.cone -> newQuanta), name)
    ret
  }

  def coneIndex(cone: Cone): Int = {
    tagMap(cone.tag).zipWithIndex.collectFirst{
      case (c, idx) if cone == c => idx
    }.get
  }

  def fullTag(cone: Cone): String = {
    val index = coneIndex(cone).toString
     cone.tag + "#" + "0" * (32 - index.length)
  }

  def tagMapSerialize(): Iterable[String] = {
    tagMap.flatMap {
      case (tag, cones) =>
        cones.zipWithIndex.map {
          case (cone, idx) => cone.serialize(s"${idx}$tag:")
        }
    }
  }

  def quantasSerialize(): Iterable[String] = {
    quantas.map {
      case (cone, quanta) =>
        fullTag(cone) + ":  " + quanta.results.toString() + ", " + quanta.mappings.toString()
    }
  }

  def prettySerialize(): String = {
    (tagMapSerialize() ++ quantasSerialize()).mkString("\n")
  }

  def hasTagConflict(tag: String): Boolean = {
    tagMap.get(tag) match {
      case Some(seq) if seq.size > 1 => true
      case other => false
    }
  }

  /** Returns a similar quanta on the same cone with a subset of bins which have not been covered
    *
    * @param quanta
    * @return
    */
  def trim(quanta: Quanta): Quanta = {
    val updated = quantas.get(quanta.cone).map(_.update(quanta)).getOrElse(quanta)

    val unseenResults = quanta.results.collect { case x@(category, (seen, labels)) if !updated.results(category)._1 => x }
    quanta.copy(results = unseenResults)
  }

  def coverage(cone: Cone): Map[BinCategory, (Boolean, Set[String])] = {
    quantas(cone).results
  }

  def targets(cone: Cone): Map[ReferenceTarget, Seq[ReferenceTarget]] = {
    quantas(cone).mappings
  }


  def writeCoverage(name: String = name): Unit = {
    writeTo(s"artifacts/coverage", s"$name.db")
  }

  def writeTo(dir: String, fileName: String): Unit = {
    val newFile = new File(dir)
    newFile.mkdirs()
    println(newFile.getAbsolutePath)
    val path = newFile.getAbsolutePath

    val file = new FileOutputStream(path + "/" + fileName)
    val out = new ObjectOutputStream(file)
    out.flush()

    // Method for serialization of object
    out.writeObject(this)

    out.flush()
    out.close()
    file.flush()
    file.close()

    val filePretty = new FileOutputStream(path + "/" + fileName + ".txt")
    val outPretty = new ObjectOutputStream(filePretty)
    filePretty.flush()
    outPretty.flush()

    // Method for serialization of object
    outPretty.writeObject(this.prettySerialize())

    outPretty.flush()
    outPretty.close()
    filePretty.flush()
    filePretty.close()

    println("Database has been serialized")
  }
}

object Database {
  def updateCoverage(name: String, messages: Seq[String], f: (Database, Seq[String]) => Database): Database = {
    val fileName = s"artifacts/coverage/$name.db"
    val fileIn = new FileInputStream(fileName)
    val in = new ObjectInputStream(fileIn)
    // Method for deserialization of object
    val database = try {
      in.readObject().asInstanceOf[Database]
    } catch {
      case e =>
        println(fileName)
        throw e
    }

    println("Database has been deserialized")

    val finalDatabase = f(database, messages)

    //val x = Database(database.quantas.map {
    //  case (cone, quanta) => quanta.cone -> quanta
    //}, database.name)


    val fileOut = new FileOutputStream(fileName)
    val out = new ObjectOutputStream(fileOut)
    out.flush()

    // Method for serialization of object
    out.writeObject(finalDatabase)
    in.close()
    fileIn.close()
    out.close()
    fileOut.close()

    finalDatabase
  }
  def populateCoverage(name: String): Database = {
    val file = new File(s"artifacts/coverage/$name.db")
    if(file.exists()) {
      readCoverage(name)
    } else {
      Database(Map.empty, name)
    }
  }
  def readCoverage(name: String): Database = {
    deserialize(s"artifacts/coverage/$name.db")
  }
  def deserialize(fileName: String): Database = {
    val file = new FileInputStream(fileName)
    val in = new ObjectInputStream(file)
    // Method for deserialization of object
    val database =
      in.readObject().asInstanceOf[Database]
    in.close()
    file.close()

    val x = Database(database.quantas.map {
      case (cone, quanta) => quanta.cone -> quanta
    }, database.name)

    println("Database has been deserialized")

    x
  }
}
