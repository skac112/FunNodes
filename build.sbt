name := "funnodes"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.3"
exportJars := true
organization := "skac"
scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"
//libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"


