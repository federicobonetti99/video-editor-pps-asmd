import Dependencies._

ThisBuild / scalaVersion     := "3.3.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "video-editor-pps-asmd",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
