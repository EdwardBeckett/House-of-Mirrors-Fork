

name := "Mouse of Horrors"

version := "2.0"

organization := "com.github.som-snytt"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-P:continuations:enable")

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _)

libraryDependencies ++= Seq(
    "org.scala-lang" %% "scala-react" % "2.9.1",
    "org.scalaz" %% "scalaz-core" % "6.0.3",
    "org.scalatest" %% "scalatest" % "1.6.1",
    "com.novocode" % "junit-interface" % "0.6" % "test->default",
    "com.maqicode.util" %% "scala-testing-support" % "0.1" % "test",
    "junit" % "junit" % "4.8.2" % "test"
)
