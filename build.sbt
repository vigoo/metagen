import xerial.sbt.Sonatype._

name := "metagen"

lazy val commonSettings = Seq(
  organization           := "io.github.vigoo",
  scalaVersion           := "2.13.7",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
  publishMavenStyle      := true,
  licenses               := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  sonatypeProjectHosting := Some(GitHubHosting("vigoo", "metagen", "daniel.vigovszky@gmail.com")),
  developers             := List(
    Developer(
      id = "vigoo",
      name = "Daniel Vigovszky",
      email = "daniel.vigovszky@gmail.com",
      url = url("https://vigoo.github.io")
    )
  ),
  credentials ++=
    (for {
      username <- Option(System.getenv().get("SONATYPE_USERNAME"))
      password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
    } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

lazy val root = Project("metagen", file("."))
  .settings(commonSettings)
  .settings(publishArtifact := false)
  .aggregate(core)

lazy val core = Project("metagen-core", file("metagen-core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta"        % "4.4.29",
      "org.scalameta" %% "scalafmt-dynamic" % "3.0.6",
      "dev.zio"       %% "zio"              % "1.0.12",
      "dev.zio"       %% "zio-prelude"      % "1.0.0-RC7",
      "dev.zio"       %% "zio-nio"          % "1.0.0-RC11"
    )
  )
