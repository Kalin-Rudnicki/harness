//

// =====| Shared Settings |=====

val Scala_3 = "3.1.2-RC3"

val MyOrg = "io.github.kalin-rudnicki"

ThisBuild / watchBeforeCommand := Watch.clearScreen

lazy val miscSettings =
  Seq(
    scalaVersion := Scala_3,
    scalacOptions += "-source:future",
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
  )

lazy val publishSettings =
  Seq(
    organization := MyOrg,
  )

lazy val testSettings =
  Seq(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )

// =====| Projects |=====

lazy val `harness-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      `harness-test`.js,
      `harness-test`.jvm,
      `harness-core`.js,
      `harness-core`.jvm,
      `harness-cli`.js,
      `harness-cli`.jvm,
      `harness-zio`.js,
      `harness-zio`.jvm,
      `harness-web`.js,
      `harness-web`.jvm,
    )

lazy val `harness-test` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-test"))
    .settings(
      name := "harness-test",
      publishSettings,
      miscSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.8.0",
        "dev.zio" %%% "zio-test" % "2.0.0",
        "dev.zio" %%% "zio-test-sbt" % "2.0.0",
      ),
    )

lazy val `harness-core` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-core"))
    .settings(
      name := "harness-core",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.8.0",
      ),
    )
    .dependsOn(`harness-test` % Test)

lazy val `harness-cli` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-cli"))
    .settings(
      name := "harness-cli",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`harness-core` % "test->test;compile->compile")

lazy val `harness-zio` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-zio"))
    .settings(
      name := "harness-zio",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio" % "2.0.0",
      ),
    )
    .dependsOn(`harness-cli` % "test->test;compile->compile")

lazy val `harness-web` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-web"))
    .settings(
      name := "harness-web",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio-json" % "0.3.0-RC10",
      ),
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "scalatags" % "0.11.1",
        "io.github.cquiroz" %%% "scala-java-time" % "2.3.0",
        "com.github.julien-truffaut" %%% "monocle-core" % "3.0.0-M6",
        "com.github.julien-truffaut" %%% "monocle-macro" % "3.0.0-M6",
      ),
    )
    .dependsOn(`harness-zio` % "test->test;compile->compile")
