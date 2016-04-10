name := "MathLanguage_Eclipse"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-language:implicitConversions")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )
