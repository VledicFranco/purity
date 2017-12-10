# Purity

[![Build Status](https://travis-ci.org/FrancoAra/purity.svg?branch=master)](https://travis-ci.org/FrancoAra/purity)
[![Join the chat at https://gitter.im/francoara/purity](https://badges.gitter.im/francoara/purity.svg)](https://gitter.im/francoara/purity?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/FrancoAra/purity.svg?columns=backlog,in%20progress,review,done)](https://waffle.io/FrancoAra/purity)
[![Maven Central](https://img.shields.io/maven-central/v/com.francoara/purity-core_2.12.svg)](http://search.maven.org/#search|ga|1|com.francoara.purity)

All documentation is available on the [Purity website](http://francoara.github.io/purity/), as well as the [api](http://francoara.github.io/purity/api) docs.

## Overview

Purity is a small library that provides abstractions that enable programmers to write 100% pure fp programs in Scala.

Purity's objective is to provide an easy to use and easy to understand api, so that any programmer, independently of
his/her level of knowledge in fp or math, may be able to design and build complex applications that benefit from
the properties of referentially transparent, typeful code. Such objective is achieved through delivering abstractions
that solve every day program staple requirements, like dependency injection, failure handling or modular design.

Purity's abstractions design is focused on using Scala's type system to help you create correct programs which parts are easy
to replace, built on top of [Cats](https://typelevel.org/cats/), they are designed to be the glue between the business
domain logic, and all of our beloved category theory concepts.

## Getting Started

Purity uses `cats-core 1.0.0-MF` and `cats-effect 0.5`, it is published to Maven Central, you can just add the following to
your build.sbt file:

```scala
libraryDependencies += "com.francoara" %% "purity-core" % "0.1.0"
```

## Copyright and license

All code is available to you under the MIT license, available [here](https://opensource.org/licenses/mit-license.php).
