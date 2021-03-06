import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.cross.{ CrossProject, CrossType }
import ReleaseTransformations._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys
import MimaKeys.{ previousArtifacts, binaryIssueFilters }

import com.typesafe.sbt.SbtGit._
import GitKeys._

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.6", "2.11.7", "2.12.0-M3")
)

addCommandAlias("root", ";project rootJVM")
addCommandAlias("common", ";project commonJVM")
addCommandAlias("scalacheck", ";project scalacheckJVM")
addCommandAlias("scalaz", ";project scalazJVM")
addCommandAlias("spire", ";project spireJVM")

addCommandAlias("validate", ";root;compile;test")
addCommandAlias("validateJVM", ";project rootJVM;compile;test")
addCommandAlias("validateJS", ";project rootJS;compile;test")

addCommandAlias("releaseAll", ";root;release skip-tests")

val shapelessVersion = "2.2.0"
val scalacheckVersion = "1.11.3"
val scalazVersion = "7.1.0"
val spireVersion = "0.8.2"
val scalatestVersion = "2.1.3"
val specs2Version = "2.4"
val scalazSpecs2Version = "0.3.0"

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Xfatal-warnings",
    "-deprecation",
    "-unchecked"
  ),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  scalacOptions in console in Compile -= "-Xfatal-warnings",
  scalacOptions in console in Test    -= "-Xfatal-warnings",

  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/typelevel/shapeless-contrib"),
      "scm:git:git://github.com/typelevel/shapeless-contrib.git"
    ))
) ++ scalaMacroDependencies

lazy val commonJsSettings = Seq(
  scalaJSUseRhino in Global := false,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false
)

lazy val coreSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val root = project.in(file("."))
  .aggregate(rootJS, rootJVM)
  .dependsOn(rootJS, rootJVM)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val rootJVM = project.in(file(".rootJVM"))
  .aggregate(commonJVM, scalacheckJVM, scalazJVM, spireJVM)
  .dependsOn(commonJVM, scalacheckJVM, scalazJVM, spireJVM)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val rootJS = project.in(file(".rootJS"))
  .aggregate(commonJS, scalacheckJS, scalazJS, spireJS)
  .dependsOn(commonJS, scalacheckJS, scalazJS, spireJS)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val common = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "shapeless-contrib-common")
  .settings(coreSettings:_*)
  .settings(mimaSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai"    %%% "shapeless"  % shapelessVersion
    )
  )
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val commonJVM = common.jvm
lazy val commonJS = common.js

lazy val scalacheck = crossProject.crossType(CrossType.Pure)
  .dependsOn(common)
  .settings(moduleName := "shapeless-scalacheck")
  .settings(coreSettings:_*)
  .settings(mimaSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai"    %%% "shapeless"  % shapelessVersion,
      "org.scalacheck" %%% "scalacheck" % scalacheckVersion
    )
  )
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val scalacheckJVM = scalacheck.jvm
lazy val scalacheckJS = scalacheck.js

lazy val scalaz = crossProject.crossType(CrossType.Pure)
  .dependsOn(common, scalacheck % "test")
  .settings(moduleName := "shapeless-scalaz")
  .settings(coreSettings:_*)
  .settings(mimaSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai"    %%% "shapeless"                 % shapelessVersion,
      "org.scalaz"     %%% "scalaz-core"               % scalazVersion,
      "org.specs2"     %%% "specs2"                    % specs2Version       % "test",
      "org.scalacheck" %%% "scalacheck"                % scalacheckVersion   % "test",
      "org.scalaz"     %%% "scalaz-scalacheck-binding" % scalazVersion       % "test",
      "org.typelevel"  %%% "scalaz-specs2"             % scalazSpecs2Version % "test"
    )
  )
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val scalazJVM = scalaz.jvm
lazy val scalazJS = scalaz.js

lazy val spire = crossProject.crossType(CrossType.Pure)
  .dependsOn(common, scalacheck % "test")
  .settings(moduleName := "shapeless-spire")
  .settings(coreSettings:_*)
  .settings(mimaSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai"    %%% "shapeless"                % shapelessVersion,
      "org.scala-lang" %   "scala-reflect"            % scalaVersion.value % "provided",
      "org.spire-math" %%% "spire"                    % spireVersion,
      "org.scalatest"  %%% "scalatest"                % scalatestVersion   % "test",
      "org.scalacheck" %%% "scalacheck"               % scalacheckVersion  % "test",
      "org.spire-math" %%% "spire-scalacheck-binding" % spireVersion       % "test"
    )
  )
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val spireJVM = spire.jvm
lazy val spireJS = spire.js

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      case Some((2, 10)) =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))
    }
  }
)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  homepage := Some(url("http://typelevel.org/")),
  licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php")),
  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/typelevel/shapeless-contrib"),
      "scm:git:git://github.com/typelevel/shapeless-contrib.git"
    )),
  pomExtra := (
    <developers>
      <developer>
        <id>larsrh</id>
        <name>Lars Hupel</name>
        <url>https://github.com/larsrh</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val mimaSettings = mimaDefaultSettings ++ Seq(
  previousArtifacts := Set(), // Set(organization.value %% moduleName.value % "2.3.0"),

  binaryIssueFilters ++= {
    import com.typesafe.tools.mima.core._
    import com.typesafe.tools.mima.core.ProblemFilters._

    // Filtering the methods that were added since the checked version
    // (these only break forward compatibility, not the backward one)
    Seq(
    )
  }
)

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
