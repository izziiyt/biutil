organization := "izziiyt"
version := "1.1.0-SNAPSHOT"
name := "biutil"
scalaVersion := Common.scalaVersion
crossScalaVersions  := Common.crossScalaVersions
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.scalanlp" %% "breeze" % "0.11.2"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case x if x.startsWith("2.11") =>
      deps :+ "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
    case _       =>
      deps
  }
}

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

scalacOptions ++= Seq(
  "-optimize",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:implicitConversions"
)

target in Compile in doc := baseDirectory.value.getParentFile / "izziiyt.github.io/biutil/api"