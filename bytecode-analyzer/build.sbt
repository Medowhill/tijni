ThisBuild / scalaVersion := "2.13.2"
ThisBuild / organization := "info.hjaem"
ThisBuild / name := "bytecodeanalyzer"

ThisBuild / scalacOptions += "-deprecation"

ThisBuild / libraryDependencies += "org.ow2.asm" % "asm" % "8.0.1"
ThisBuild / libraryDependencies += "org.ow2.asm" % "asm-tree" % "8.0.1"
ThisBuild / libraryDependencies += "org.ow2.asm" % "asm-util" % "8.0.1"
ThisBuild / libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.0"
