

lazy val root = (project in file("."))
  .settings(
    organization := "com.github.agetakoyaki29",
    name := "geo1",
    version := "1.0",
    scalaVersion := "2.11.8",

    libraryDependencies += junit,
    // libraryDependencies += junitinterface,
    libraryDependencies += scalactic,
    libraryDependencies += scalatest,

    logBuffered in Test := false
  )

// ---- lib ----
lazy val junit = "junit" % "junit" % "4.11" % "test"
lazy val junitinterface = "com.novocode" % "junit-interface" % "0.11" % "test"
lazy val scalactic = "org.scalactic" %% "scalactic" % "2.2.5"
lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.5" % "test"
