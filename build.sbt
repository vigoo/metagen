name := "metagen"

lazy val commonSettings = Seq(
  organization := "io.github.vigoo",
  scalaVersion := "2.13.6",
  version := "0.1",
  scalaVersion := "2.13.6",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
)

lazy val root = Project("metagen", file("."))
  .settings(commonSettings)
  .settings(publishArtifact := false)
  .aggregate(core)

lazy val core = Project("metagen-core", file("metagen-core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta"        % "4.4.27",
      "org.scalameta" %% "scalafmt-dynamic" % "3.0.2",
      "dev.zio"       %% "zio"              % "1.0.11",
      "dev.zio"       %% "zio-prelude"      % "1.0.0-RC6",
      "dev.zio"       %% "zio-nio"          % "1.0.0-RC11"
    )
  )
