def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

updateOptions := updateOptions.value.withLatestSnapshots(true)

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.11.12", "2.12.4")

scalacOptions := Seq("-deprecation", "-feature") ++ scalacOptionsVersion(scalaVersion.value)

javacOptions ++= javacOptionsVersion(scalaVersion.value)

lazy val commonSettings = Seq (
  organization := "edu.berkeley.cs",
  scalaVersion := "2.12.4",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),

  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1"
  ),
)



//val commonSettings = Seq(
//  scalaVersion := "2.12.4",
//  crossScalaVersions := Seq("2.11.12", "2.12.4"),
//  resolvers ++= Seq(
//    Resolver.sonatypeRepo("snapshots"),
//    Resolver.sonatypeRepo("releases")
//  )
//)

val srcSettings = commonSettings ++ Seq(
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "treadle" % "1.1-SNAPSHOT",
    "edu.berkeley.cs" %% "chisel-iotesters" % "1.3-SNAPSHOT"
  )
)

val miniSettings = commonSettings ++ Seq(
  name := "riscv-mini",
  version := "2.0-SNAPSHOT",
  organization := "edu.berkeley.cs"
)

lazy val firrtl  = project in file("./firrtl") settings commonSettings
lazy val chisel  = project in file("./chisel3") settings commonSettings settings srcSettings dependsOn firrtl
lazy val lib  = project in file("./riscv-mini/lib") settings commonSettings settings srcSettings dependsOn chisel dependsOn firrtl
lazy val mini = project in file("./riscv-mini")  settings miniSettings settings srcSettings dependsOn lib dependsOn chisel dependsOn firrtl
lazy val coverage  = project in file(".") settings commonSettings settings srcSettings dependsOn (mini % "test->test;test->compile;compile->test") dependsOn chisel dependsOn (firrtl % "compile->compile;test->test")
