import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers, scalacOptions}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / version := "1.3.0-JDK-17-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.13"

val settings = Common.settings ++ Common.publish ++ Seq(
    organization := "org.mule.syaml",
    name := "syaml",
    libraryDependencies ++= Seq(
        "org.mule.common" %%% "scala-common-test" % "0.1.12" % Test
    ),
    resolvers ++= List(Common.releases, Common.snapshots, Resolver.mavenLocal),
    credentials ++= Common.credentials()
)

lazy val workspaceDirectory: File =
  sys.props.get("sbt.mulesoft") match {
    case Some(x) => file(x)
    case _       => Path.userHome / "mulesoft"
  }

val scalaCommonVersion = "1.2.0-JDK-17-SNAPSHOT"

lazy val scalaCommonJVMRef = ProjectRef(workspaceDirectory / "scala-common", "commonJVM")
lazy val scalaCommonJSRef  = ProjectRef(workspaceDirectory / "scala-common", "commonJS")
lazy val scalaCommonLibJVM = "org.mule.common" %% "scala-common" % scalaCommonVersion
lazy val scalaCommonLibJS  = "org.mule.common" %% "scala-common_sjs0.6" % scalaCommonVersion

lazy val syaml = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(settings: _*)
  .jvmSettings(
      // JVM-specific settings here
      libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  )
  .jsSettings(
      // JS-specific settings here
      scalaJSModuleKind := ModuleKind.CommonJSModule,
      scalacOptions += "-P:scalajs:suppressExportDeprecations"
  )

lazy val syamlJVM = syaml.jvm.in(file("./jvm")).sourceDependency(scalaCommonJVMRef, scalaCommonLibJVM)
lazy val syamlJS = syaml.js
  .in(file("./js"))
  .sourceDependency(scalaCommonJSRef, scalaCommonLibJS)
  .disablePlugins(SonarPlugin, ScoverageSbtPlugin)

def getVersion(major: Int, minor: Int) = {
  lazy val build  = sys.env.getOrElse("BUILD_NUMBER", "0")
  lazy val branch = sys.env.get("BRANCH_NAME")

  if (branch.contains("master")) s"$major.$minor.$build" else s"$major.${minor + 1}.0-SNAPSHOT"
}

lazy val sonarUrl   = sys.env.getOrElse("SONAR_SERVER_URL", "Not found url.")
lazy val sonarToken = sys.env.getOrElse("SONAR_SERVER_TOKEN", "Not found token.")
lazy val branch     = sys.env.getOrElse("BRANCH_NAME", "develop")

sonarProperties := Map(
    "sonar.login"             -> sonarToken,
    "sonar.projectKey"        -> "mulesoft.syaml",
    "sonar.projectName"       -> "syaml",
    "sonar.projectVersion"    -> version.value,
    "sonar.sourceEncoding"    -> "UTF-8",
    "sonar.github.repository" -> "aml-org/syaml",
    "sonar.branch.name"       -> branch,
    "sonar.sources"           -> "shared/src/main/scala",
    "sonar.tests"             -> "shared/src/test/scala",
    "sonar.userHome"          -> "${buildDir}/.sonar"
)
