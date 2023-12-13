ThisBuild / organization := "edu.berkeley.cs"
ThisBuild / version := "0.0.1-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"
ThisBuild / scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-language:reflectiveCalls",
  "-Xcheckinit",
  "-Xlint",
)

Compile / doc / scalacOptions += "-groups"

val chiselVersion = "3.6.0"

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "edu.berkeley.cs" %% "chiseltest" % "0.6.2" % Test,
  "org.scalatest" %% "scalatest" % "3.2.17" % Test,
)

// Plugins
Global / excludeLintKeys += idePackagePrefix
idePackagePrefix := Some("edu.berkeley.cs.ucie.digital")
