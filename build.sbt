import sbt.Keys.{libraryDependencies, publishTo}

name := "syaml"

scalaVersion in ThisBuild := "2.12.2"

val repository = sys.env.getOrElse("NEXUS_REPOSITORY", "https://nexus.build.msap.io/nexus")

lazy val root = project.in(file(".")).
  aggregate(syamlJS, syamlJVM)

lazy val syaml = crossProject.in(file(".")).
  settings(
    organization := "org.mulesoft",
    name := "syaml",
    version := "0.0.1-SNAPSHOT",

    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.0.1",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test",

    publishTo := Some(
      "snapshots" at s"$repository/content/repositories/ci-snapshots/"),
    credentials ++= Seq(
      Credentials("Sonatype Nexus Repository Manager",
        new java.net.URL(repository).getHost,
        sys.env.getOrElse("NEXUS_USER", ""),
        sys.env.getOrElse("NEXUS_PASS", ""))
    ),

    javacOptions ++= Seq("-encoding", "UTF-8")
  ).
  jvmSettings(
    // JVM-specific settings here
  ).
  jsSettings(
    // JS-specific settings here
  )

lazy val syamlJVM = syaml.jvm
lazy val syamlJS = syaml.js