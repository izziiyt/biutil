name := "biutil"

version := "0.1"

scalaVersion := "2.11.6"

organization := "izzii"

libraryDependencies  ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.1.3" % "test"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)