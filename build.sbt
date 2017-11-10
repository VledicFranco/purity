import sbt._
import sbt.Keys._

val cats        = "org.typelevel"  %% "cats-core"   % "1.0.0-RC1"
val catsLaws    = "org.typelevel"  %% "cats-laws"   % "1.0.0-RC1"
val catsEffect  = "org.typelevel"  %% "cats-effect" % "0.5"
val shapeless   = "com.chuusai"    %% "shapeless"   % "2.3.2"
val discipline  = "org.typelevel"  %% "discipline"  % "0.8"
val scalaTest   = "org.scalatest"  %% "scalatest"   % "3.0.4"
val scalacheck  = "org.scalacheck" %% "scalacheck"  % "1.13.5"

lazy val root = Project("purity", file("."))
  .settings(
    organization := "purity",
    scalaVersion := "2.12.4",
    libraryDependencies ++= Seq(
      cats,
      catsEffect,
      shapeless,
      scalaTest % Test,
      scalacheck % Test,
      discipline % Test,
      catsLaws % Test
    ),
    scalacOptions ++= Seq(
      "-language:higherKinds",
      "-Ypartial-unification"
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
  )
