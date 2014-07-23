name := """scala-genetic"""

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.1"

EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.6" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.3.3"
)
