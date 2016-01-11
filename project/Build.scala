import sbt._
import Keys._

object BuildLearning extends Build {

  val commonDependencies = Seq(
    "org.scalatest"  %% "scalatest" % "2.2.2" % "test",
    "org.scalamock"  %% "scalamock-scalatest-support" % "3.2" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test")

  val commonSettings = Seq(
    libraryDependencies ++= commonDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"))

  val javaFxSettings = Seq(
    libraryDependencies += "org.scalafx" %% "scalafx" % "1.0.0-M3",
    unmanagedJars in Compile += Attributed.blank(file(
      "/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home/jre/lib/ext/jfxrt.jar")))

  lazy val algorithms = project
    .in(file("algorithms"))
    .settings(Seq(version := "0.1") ++ commonSettings)
    .dependsOn(layout)

  lazy val monads = project
    .in(file("monads"))
    .settings(Seq(version := "0.1") ++ commonSettings)

  lazy val math = project
    .in(file("math"))
    .settings(Seq(version := "0.1") ++ commonSettings)

  lazy val layout = project
    .in(file("layout"))
    .settings(Seq(version := "0.1") ++ commonSettings)

  lazy val expressions = project
    .in(file("expressions"))
    .settings(Seq(version := "0.1") ++ commonSettings)
    .dependsOn(layout)

  lazy val codeReview = project
    .in(file("codeReview"))
    .settings(Seq(version := "0.1") ++ commonSettings)

  lazy val graphics = project
    .in(file("graphics"))
    .settings(Seq(version := "0.1") ++ commonSettings ++ javaFxSettings)

  lazy val root = Project(
    id        = "root",
    base      = file("."),
    aggregate = Seq(algorithms, monads, math, layout, expressions, codeReview, graphics),
    settings  = commonSettings ++ Seq(
      Keys.`package` :=  file(""),
      packageBin in Global :=  file(""),
      packagedArtifacts :=  Map()))
}
