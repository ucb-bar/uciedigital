// ThisBuild / organization := "edu.berkeley.cs"
// ThisBuild / version := "0.0.1-SNAPSHOT"

// ThisBuild / scalaVersion := "2.13.10"
// ThisBuild / scalacOptions := Seq(
//   "-deprecation",
//   "-feature",
//   "-language:reflectiveCalls",
//   "-Xcheckinit",
//   "-Xlint",
// )

// Compile / doc / scalacOptions += "-groups"

// val chiselVersion = "3.6.0"

// lazy val root = (project in file("."))
//   .settings(
//     name := "uciedigital",
//     libraryDependencies ++= Seq(
//       "edu.berkeley.cs" %% "chisel3" % chiselVersion,
//       "edu.berkeley.cs" %% "chiseltest" % "0.6.2" % Test,
//       "org.scalatest" %% "scalatest" % "3.2.17" % Test,
//       "edu.berkeley.cs" %% "rocketchip" % "1.6.0",
//       "edu.berkeley.cs" %% "rocket-macros" % "1.6.0",
//       "edu.berkeley.cs" %% "cde" % "1.6.0",
//     ),
//     addCompilerPlugin(
//       "edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full,
//     ),
//   )

// // Plugins
// Global / excludeLintKeys += idePackagePrefix
// root / idePackagePrefix := Some("edu.berkeley.cs.ucie.digital")

name := "ucie_digital"
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

// SNAPSHOT repositories
libraryDependencies ++=
  Seq(
    "edu.berkeley.cs" %% "rocketchip-3.6.0" % "1.6-3.6.0-e3773366a-SNAPSHOT",
    "edu.berkeley.cs" %% "chisel3" % chiselVersion,
    "edu.berkeley.cs" %% "chiseltest" % "0.6.2" % "test",
    "org.scalatest" %% "scalatest" % "3.2.18" % "test",
  )

resolvers ++= Resolver.sonatypeOssRepos("snapshots")
resolvers ++= Resolver.sonatypeOssRepos("releases")
resolvers += Resolver.mavenLocal
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/service/local/repositories/snapshots/content"

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)

import Tests._

Test / fork := true
Test / testGrouping := (Test / testGrouping).value.flatMap { group =>
   group.tests.map { test =>
      Group(test.name, Seq(test), SubProcess(ForkOptions()))
   }
}

concurrentRestrictions := Seq(Tags.limit(Tags.ForkedTestGroup, 72))
