lazy val commonSettings = Seq(
  organization := "izzii",
  version := "1.0.0",
  name := "biutil",
  scalaVersion := "2.10.5"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies  ++= Seq(
      "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test",
      "org.scalanlp" %% "breeze" % "0.11.2"
    ),
    resolvers ++= Seq(
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    ),
    scalacOptions ++= Seq(
      "-optimize",
      "-deprecation",
      "-feature",
      "-unchecked"
    )
  )


