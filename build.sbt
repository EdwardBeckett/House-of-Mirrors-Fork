
name := "Mouse of Horrors"

version := "1.0"

organization := "com.github.som-snytt"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _)

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.9.0" % "1.6.1",
    "com.novocode" % "junit-interface" % "0.6" % "test->default",
    "com.google.inject" % "guice" % "2.0"
)
