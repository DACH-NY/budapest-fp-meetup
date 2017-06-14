
name := "scheme-scala"
version := "0.0.1-SNAPSHOT"
organization := "com.digitalasset"
scalaVersion := "2.12.2"

scalacOptions ++= Seq(
  "-feature", // doesn't allow advance features of the language without explict import (higherkinds, implicits)
  "-target:jvm-1.8",
  "-encoding",
  "UTF-8",
  "-unchecked", // more detailed information about type-erasure related warnings
  "-deprecation", // warn if using deprecated stuff
  "-Xfuture",
  "-Xlint",
  //"-Xfatal-warnings", // :trollface:
  "-Yno-adapted-args", // adapted args is a deprecated feature: `def foo(a: (A, B))` can be called with `foo(a, b)`. properly it should be `foo((a,b))`
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen", // Warn about implicit conversion between numerical types
  "-Ywarn-value-discard", // Gives a warning for functions declared as returning Unit, but the body returns a value
  "-Ywarn-unused-import"
    // unfortunately give false warning for the `(a, b) = someTuple` line inside a for comprehension
    //"-Ywarn-unused"
)

libraryDependencies ++= Seq(
  "org.scalaz"    %% "scalaz-core"          % "7.2.13",
  "org.tpolecat"  %% "atto-compat-scalaz72" % "0.5.1",
  "org.scalatest" %% "scalatest"            % "3.0.1",

  compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
)

wartremoverErrors ++= Warts.allBut(
  Wart.ArrayEquals,
  Wart.PublicInference,
  Wart.Nothing,
  Wart.Equals,
  Wart.Throw,
  Wart.NonUnitStatements
)
