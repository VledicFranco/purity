import sbt._
import sbt.Keys._

organization in ThisBuild := "com.francoara"

lazy val purity = project.in(file("."))
  .settings(moduleName := "root")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(core)

lazy val core = project.in(file("core"))
  .settings(moduleName := "purity-core", name := "Purity core")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(librarySettings)
  .settings(testSettings)

lazy val docs = project
  .dependsOn(core)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(moduleName := "purity-docs")
  .settings(commonSettings)
  .settings(docSettings)
  .settings(noPublishSettings)

lazy val commonSettings = Seq(
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-Ypartial-unification"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)

lazy val catsVersion = "1.0.0-RC1"
lazy val catsEffectVersion = "0.5"

lazy val librarySettings = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++=
    "org.typelevel"  %% "cats-core"   % catsVersion       ::
    "org.typelevel"  %% "cats-effect" % catsEffectVersion :: Nil
)

lazy val disciplineVersion = "0.8"
lazy val scalaCheckVersion = "1.13.5"
lazy val scalaTestVersion = "3.0.4"

lazy val testSettings = Seq(
  libraryDependencies ++=
    "org.typelevel"  %% "cats-laws"   % catsVersion       % Test ::
    "org.typelevel"  %% "discipline"  % disciplineVersion % Test ::
    "org.scalatest"  %% "scalatest"   % scalaTestVersion  % Test ::
    "org.scalacheck" %% "scalacheck"  % scalaCheckVersion % Test :: Nil
)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/francoara/purity")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/francoara/purity"), "scm:git:git@github.com:francoara/purity.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://typelevel.org/cats/api/")),
  pomExtra :=
    <developers>
      <developer>
        <id>francoara</id>
        <name>Francisco M. Aramburo Torres</name>
        <url>https://github.com/francoara/</url>
      </developer>
    </developers>
)

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val docSettings = Seq(
  micrositeName := "Purity",
  micrositeDescription := "Easy pure code in Scala",
  micrositeAuthor := "Francisco M. Aramburo Torres",
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "https://francoara.github.io/purity/",
  micrositeBaseUrl := "/purity",
  micrositeDocumentationUrl := "/purity/api/",
  micrositeGithubOwner := "FrancoAra",
  micrositeGithubRepo := "purity",
  micrositePalette := Map(
    "brand-primary"   -> "#5B5988",
    "brand-secondary" -> "#292E53",
    "brand-tertiary"  -> "#222749",
    "gray-dark"       -> "#49494B",
    "gray"            -> "#7B7B7E",
    "gray-light"      -> "#E5E5E6",
    "gray-lighter"    -> "#F4F3F4",
    "white-color"     -> "#FFFFFF"),
  autoAPIMappings := true,
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir),
  ghpagesNoJekyll := false,
  fork in tut := true,
  fork in (ScalaUnidoc, unidoc) := true,
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-Xfatal-warnings",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  ),
  git.remoteRepo := "git@github.com:FrancoAra/purity.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md"
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)