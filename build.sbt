import sbt._
import sbt.Keys._

organization in ThisBuild := "com.francoara"

lazy val catsVersion = "1.0.0-RC1"

lazy val http4sVersion = "0.18.0-M5"

lazy val cats = Def.setting("org.typelevel" %% "cats-core" % catsVersion)

lazy val catsEffects = Def.setting("org.typelevel"  %% "cats-effect" % "0.5")

lazy val tlConfig = Def.setting("com.typesafe" % "config" % "1.3.1")

lazy val http4sDsl = Def.setting("org.http4s"  %% "http4s-dsl" % http4sVersion)

lazy val http4sCirce = Def.setting("org.http4s" %% "http4s-circe" % http4sVersion)

lazy val http4sClient = Def.setting("org.http4s" %% "http4s-blaze-client" % http4sVersion)

lazy val catsLaws = Def.setting("org.typelevel"  %% "cats-laws" % catsVersion % Test)

lazy val discipline = Def.setting("org.typelevel" %% "discipline" % "0.8" % Test)

lazy val scalatest = Def.setting("org.scalatest" %% "scalatest" % "3.0.4"  % Test)

lazy val scalacheck = Def.setting("org.scalacheck" %% "scalacheck" % "1.13.5" % Test)

lazy val purity = project.in(file("."))
  .settings(moduleName := "root")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(core, http4s, test)
  .dependsOn(core, http4s, test % "test-internal -> test")

lazy val core = project.in(file("core"))
  .settings(moduleName := "purity-core", name := "Purity core")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(librarySettings)

lazy val docs = project.in(file("docs"))
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(moduleName := "purity-docs")
  .settings(commonSettings)
  .settings(docSettings)
  .settings(noPublishSettings)
  .dependsOn(core)

lazy val example = project.in(file("example"))
  .settings(moduleName := "purity-example")
  .settings(commonSettings)
  .settings(librarySettings)
  .settings(noPublishSettings)
  .aggregate(core, http4s)
  .dependsOn(core, http4s)

lazy val http4s = project.in(file("http4s"))
  .settings(moduleName := "purity-http4s", name := "Purity http4s")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(librarySettings)
  .settings(libraryDependencies ++= http4sDsl.value :: http4sCirce.value :: http4sClient.value :: Nil)
  .dependsOn(core)

lazy val test = project.in(file("test"))
  .settings(moduleName := "purity-test")
  .settings(commonSettings)
  .settings(librarySettings)
  .settings(testSettings)
  .settings(noPublishSettings)
  .aggregate(core, http4s)
  .dependsOn(core, http4s)

lazy val commonSettings = Seq(
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-Ypartial-unification"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)

lazy val librarySettings = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= tlConfig.value :: cats.value :: catsEffects.value :: Nil
)

lazy val testSettings =
  Seq(libraryDependencies ++= catsLaws.value :: discipline.value :: scalatest.value :: scalacheck.value :: Nil)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  homepage := Some(url("https://francoara.github.io/purity")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/francoara/purity"), "scm:git:git@github.com:francoara/purity.git")),
  autoAPIMappings := true,
  apiURL := Some(url("https://francoara.github.io/purity/api/")),
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }.value,
  developers := List(
    Developer(
      id = "francoara",
      name = "Francisco M. Arámburo Torres",
      email = "atfm05@gmail.com",
      url = url("https://github.com/FrancoAra")
    )
  ),
  useGpg := true
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
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
