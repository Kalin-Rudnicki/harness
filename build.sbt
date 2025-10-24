//

import scala.sys.process.*

// =====| Shared Settings |=====

enablePlugins(GitVersioning)
git.gitTagToVersionNumber := { tag =>
  if (tag.matches("^\\d+\\..*$")) Some(tag)
  else None
}

val Scala_3 = "3.3.0"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "harness"

ThisBuild / watchBeforeCommand := Watch.clearScreen

lazy val testAndCompile = "test->test;compile->compile"

def miscSettings =
  Seq(
    scalaVersion := Scala_3,
    scalacOptions += "-source:future",
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.mavenCentral,
      Resolver.sonatypeCentralRepo("public"),
    ),
  )

def publishSettings =
  Seq(
    organization := MyOrg,
    description := "Miscellaneous libraries/utilities for Scala.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    homepage := Some(url(s"https://github.com/$githubUsername/$githubProject")),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
    version := { // TODO (KR) : I hate doing this, but one/both of the git plugins seems to be bricked. Remove if they figure their shit out.
      (for {
        gitDesc <-
          try { Some(List("git", "describe", "--tags", "--exact-match").!!.trim) }
          catch { case _: Throwable => None }
        tagV <-
          if (gitDesc.matches("^[0-9]+\\..*$")) Some(gitDesc)
          else None
      } yield tagV).getOrElse(version.value)
    },
  )

def testSettings =
  Seq(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )

// =====| Projects |=====

lazy val `harness-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
      organization := MyOrg,
    )
    .aggregate(
      `harness-test`.js,
      `harness-test`.jvm,
      `harness-zio-test`.js,
      `harness-zio-test`.jvm,
      `harness-core`.js,
      `harness-core`.jvm,
      `harness-csv`.js,
      `harness-csv`.jvm,
      `harness-xml`,
      `harness-cli`.js,
      `harness-cli`.jvm,
      `harness-zio`.js,
      `harness-zio`.jvm,
      `harness-pk`.js,
      `harness-pk`.jvm,
      `harness-email`.jvm,
      `harness-email`.js,
      `harness-payments`.jvm,
      `harness-payments`.js,
      `harness-docker`,
      `harness-docker-sql`,
      `harness-docker-kafka`,
      `harness-sql`,
      `harness-kafka`,
      `harness-web`.js,
      `harness-web`.jvm,
      `harness-http-client`.js,
      `harness-http-client`.jvm,
      `harness-http-server`,
      `harness-http-server-test`,
      `harness-web-ui`,
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

lazy val `harness-zio-test` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-zio-test"))
    .settings(
      name := "harness-zio-test",
      publishSettings,
      miscSettings,
    )
    .dependsOn(
      `harness-zio` % testAndCompile,
      `harness-test` % testAndCompile,
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

lazy val `harness-csv` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-csv"))
    .settings(
      name := "harness-csv",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-core` % testAndCompile,
    )

lazy val `harness-xml` =
  project
    .in(file("harness-xml"))
    .settings(
      name := "harness-xml",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-xml" % "2.0.0",
      ),
    )
    .dependsOn(
      `harness-core`.jvm % testAndCompile,
    )

lazy val `harness-cli` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-cli"))
    .settings(
      name := "harness-cli",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-core` % testAndCompile,
    )

lazy val `harness-zio` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-zio"))
    .settings(
      name := "harness-zio",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio" % "2.0.20",
        "dev.zio" %%% "zio-json" % "0.6.2",
      ),
    )
    .jvmSettings(
      Test / fork := true,
    )
    .dependsOn(
      `harness-cli` % testAndCompile,
    )

lazy val `harness-docker` =
  project
    .in(file("harness-docker"))
    .settings(
      name := "harness-docker",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio`.jvm % testAndCompile,
    )

lazy val `harness-docker-sql` =
  project
    .in(file("harness-docker-sql"))
    .settings(
      name := "harness-docker-sql",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-docker` % testAndCompile,
      `harness-sql` % testAndCompile,
    )

lazy val `harness-docker-kafka` =
  project
    .in(file("harness-docker-kafka"))
    .settings(
      name := "harness-docker-kafka",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-docker` % testAndCompile,
      `harness-kafka` % testAndCompile,
    )

lazy val `harness-pk` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-pk"))
    .settings(
      name := "harness-pk",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio` % testAndCompile,
    )

lazy val `harness-sql` =
  project
    .in(file("harness-sql"))
    .settings(
      name := "harness-sql",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %% "shapeless3-deriving" % "3.3.0",
        "org.postgresql" % "postgresql" % "42.5.0",
      ),
      Test / fork := true,
    )
    .dependsOn(
      `harness-pk`.jvm % testAndCompile,
    )

lazy val `harness-kafka` =
  project
    .in(file("harness-kafka"))
    .settings(
      name := "harness-kafka",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio-kafka" % "2.7.1",
      ),
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio`.jvm % testAndCompile,
    )

lazy val `harness-email` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-email"))
    .settings(
      name := "harness-email",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.sun.mail" % "javax.mail" % "1.6.2",
      ),
    )
    .dependsOn(
      `harness-zio` % testAndCompile,
    )

lazy val `harness-payments` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-payments"))
    .settings(
      name := "harness-payments",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.stripe" % "stripe-java" % "24.11.0",
      ),
    )
    .jsConfigure(_.dependsOn(`harness-web-ui` % testAndCompile))
    .dependsOn(
      `harness-email` % testAndCompile,
      `harness-pk` % testAndCompile,
    )

lazy val `harness-web` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-web"))
    .settings(
      name := "harness-web",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "scalatags" % "0.11.1",
        "io.github.cquiroz" %%% "scala-java-time" % "2.3.0",
        "com.github.julien-truffaut" %%% "monocle-core" % "3.0.0-M6",
        "com.github.julien-truffaut" %%% "monocle-macro" % "3.0.0-M6",
      ),
    )
    .dependsOn(
      `harness-pk` % testAndCompile,
    )

lazy val `harness-http-client` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-http-client"))
    .settings(
      name := "harness-http-client",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-web` % testAndCompile,
    )

lazy val `harness-http-server` =
  project
    .in(file("harness-http-server"))
    .settings(
      name := "harness-http-server",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web`.jvm % testAndCompile,
    )

lazy val `harness-http-server-test` =
  project
    .in(file("harness-http-server-test"))
    .settings(
      name := "harness-http-server-test",
      publishSettings,
      miscSettings,
    )
    .dependsOn(
      `harness-test`.jvm % testAndCompile,
      `harness-http-server` % testAndCompile,
      `harness-sql` % testAndCompile,
    )

lazy val `harness-web-ui` =
  project
    .in(file("harness-web-ui"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "harness-web-ui",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-http-client`.js % testAndCompile,
    )
