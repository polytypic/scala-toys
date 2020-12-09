lazy val root =
  project
    .in(file("."))
    .settings(settings)
    .settings(
      libraryDependencies ++= Seq(
        ),
      publishArtifact := false
    )

lazy val settings =
  Seq(
    name := "Scala experiments",
    scalaVersion := "2.13.4",
    organization := "com.polytypic",
    scalacOptions in (Compile, compile) ++= Seq("-Ywarn-unused", "-deprecation")
  )
