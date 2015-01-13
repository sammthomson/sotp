name := "tombinator"

organization := "com.samthomson"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.5"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  // parboiled 2.0.1 doesn't have the nice short combinator names, nor `cut` op
  "org.parboiled" %% "parboiled" % "2.0.2-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
