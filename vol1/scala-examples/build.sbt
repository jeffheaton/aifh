// set the name of the project
name := "AIFH Vol1 Scala examples"

version := "1.0"

organization := "com.heatonresearch.aifh"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "net.sf.opencsv" % "opencsv" % "2.3",
  "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test",
  "org.hamcrest" % "hamcrest-all" % "1.3" % "test",
  "gov.nist.math" % "jama" % "1.0.3")

scalacOptions ++= Seq(
  //"-optimize",
  "-feature",
  "-deprecation",
  "-unchecked",
  //"-Xlint",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-nullary-unit",
  //"-Ywarn-numeric-widen",
  //"-Ywarn-value-discard",
  "-Ywarn-nullary-override")
