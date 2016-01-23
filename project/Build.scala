import sbt._
import Keys._

object BuildLearning extends Build {

  val AkkaVersion = "2.4.1"

  val commonDependencies = Seq(
    "org.scalatest"  %% "scalatest" % "2.2.2" % "test",
    "org.scalamock"  %% "scalamock-scalatest-support" % "3.2" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test")

  val commonSettings = Seq(
    scalaVersion := "2.11.7",
    libraryDependencies ++= commonDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"))

  val akkaSettings = Seq(libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % AkkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % "test"))

  val javaFxSettings = Seq(
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.60-R9",
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
    .settings(Seq(version := "0.1") ++ commonSettings ++ javaFxSettings ++ akkaSettings)
    .dependsOn(math)

  lazy val bugReport = project
    .in(file("bugReport"))
    .settings(Seq(version := "0.1") ++ commonSettings)

  lazy val root = Project(
    id        = "root",
    base      = file("."),
    aggregate = Seq(algorithms, monads, math, layout, expressions, codeReview, graphics),
    settings  = commonSettings ++ Seq(
      Keys.`package` :=  file(""),
      packageBin in Global :=  file(""),
      packagedArtifacts :=  Map()))
}
