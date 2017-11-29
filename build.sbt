import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}

name := "syaml"

val settings = Common.settings ++ Seq(
  name := "syaml",
  version := "0.0.8-SNAPSHOT",

  libraryDependencies ++= Seq(
    "org.mulesoft"  %%% "scala-common" % "0.1.0",
    "org.scalactic" %%% "scalactic" % "3.0.1",
    "org.scalatest" %%% "scalatest" % "3.0.0" % Test
  ),

  Common.publish,
  resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),

  credentials ++= Common.credentials()
)

lazy val root = project.in(file(".")).aggregate(syamlJS, syamlJVM)

lazy val syaml = crossProject
  .in(file("."))
  .settings(
    settings: _*
  )
  .jvmSettings(
    // JVM-specific settings here
  )
  .jsSettings(
    // JS-specific settings here
      scalaJSModuleKind := ModuleKind.CommonJSModule
  )

lazy val syamlJVM = syaml.jvm
lazy val syamlJS = syaml.js
