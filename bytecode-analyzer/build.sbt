ThisBuild / scalaVersion := "2.13.2"
ThisBuild / organization := "info.hjaem"
ThisBuild / name := "bytecodeanalyzer"

ThisBuild / scalacOptions += "-deprecation"

val asmVersion = "8.0.1"
ThisBuild / libraryDependencies += "org.ow2.asm" % "asm" % asmVersion
ThisBuild / libraryDependencies += "org.ow2.asm" % "asm-tree" % asmVersion
ThisBuild / libraryDependencies += "org.ow2.asm" % "asm-util" % asmVersion
ThisBuild / libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.0"
ThisBuild / libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
