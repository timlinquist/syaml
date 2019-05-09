import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

name := "syaml"

val settings = Common.settings ++ Common.publish ++ Seq(
  organization := "org.mule.syaml",
  name := "syaml",
  version := {
    val major = 0
    val minor = 7

    lazy val build  = sys.env.getOrElse("BUILD_NUMBER", "0")
    lazy val branch = sys.env.get("BRANCH_NAME")

    if (branch.contains("master")) s"$major.$minor.$build" else s"$major.${minor + 1}.0-SNAPSHOT"
  },

  libraryDependencies ++= Seq(
    "org.mule.common" %%% "scala-common" % "0.5.59",
    "org.scalatest" %%% "scalatest" % "3.0.0" % Test
  ),
    
  resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),

  credentials ++= Common.credentials()
)

lazy val syaml = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(settings: _*)
  .jvmSettings(
    // JVM-specific settings here
    libraryDependencies += "org.scala-js"           %% "scalajs-stubs"          % scalaJSVersion % "provided"
  )
  .jsSettings(
    // JS-specific settings here
      scalaJSModuleKind := ModuleKind.CommonJSModule
  )

lazy val syamlJVM = syaml.jvm
lazy val syamlJS = syaml.js
