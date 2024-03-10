//

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

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

lazy val testAndCompile = "test->test;compile->compile"

lazy val miscSettings =
  Seq(
    scalaVersion := Scala_3,
    scalacOptions ++= Seq("-source:future", "-Ycheck-all-patmat", "-Wunused:all", "-Werror", "-language:implicitConversions", "-deprecation", "-feature"),
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
  )

lazy val publishSettings =
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
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
  )

lazy val testSettings =
  Seq(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )

lazy val ZioJsonVersion = "0.6.2"

// =====| Projects |=====

lazy val `harness-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
      organization := MyOrg,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    )
    .aggregate(
      `harness-modules`,
      `harness-web-app-template`,
      `harness-archive`,
    )

lazy val `harness-modules` =
  project
    .in(file("modules"))
    .settings(
      publish / skip := true,
      organization := MyOrg,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
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
      `harness-email-model`.jvm,
      `harness-email-model`.js,
      `harness-email`,
      `harness-payments`.jvm,
      `harness-payments`.js,
      `harness-sql`,
      `harness-kafka`,
      `harness-web`.js,
      `harness-web`.jvm,
      `harness-http-client`.js,
      `harness-http-client`.jvm,
      `harness-http-server`,
      `harness-web-ui`,
      `harness-js-plugin`,
      `harness-web-app-template-plugin`,
    )

lazy val `harness-test` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-test"))
    .settings(
      name := "harness-test",
      publishSettings,
      miscSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.8.0",
        "dev.zio" %%% "zio-test" % "2.0.20",
        "dev.zio" %%% "zio-test-sbt" % "2.0.20",
      ),
    )

lazy val `harness-zio-test` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-zio-test"))
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
    .in(file("modules/harness-core"))
    .settings(
      name := "harness-core",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.8.0",
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`harness-test` % Test)

lazy val `harness-csv` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-csv"))
    .settings(
      name := "harness-csv",
      publishSettings,
      miscSettings,
      testSettings,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(
      `harness-core` % testAndCompile,
    )

lazy val `harness-xml` =
  project
    .in(file("modules/harness-xml"))
    .settings(
      name := "harness-xml",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-xml" % "2.0.0",
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(
      `harness-core`.jvm % testAndCompile,
    )

lazy val `harness-cli` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-cli"))
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
    .in(file("modules/harness-zio"))
    .settings(
      name := "harness-zio",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio" % "2.0.20",
        "dev.zio" %%% "zio-json" % ZioJsonVersion,
      ),
    )
    .jvmSettings(
      Test / fork := true,
    )
    .dependsOn(
      `harness-cli` % testAndCompile,
    )

lazy val `harness-pk` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-pk"))
    .settings(
      name := "harness-pk",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio-json" % ZioJsonVersion,
      ),
    )
    .dependsOn(
      `harness-core` % testAndCompile,
    )

lazy val `harness-sql` =
  project
    .in(file("modules/harness-sql"))
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
      `harness-zio`.jvm % testAndCompile,
      `harness-pk`.jvm % testAndCompile,
    )

lazy val `harness-kafka` =
  project
    .in(file("modules/harness-kafka"))
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

lazy val `harness-email-model` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-email-model"))
    .settings(
      name := "harness-email-model",
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

lazy val `harness-email` =
  project
    .in(file("modules/harness-email"))
    .settings(
      name := "harness-email",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-email-model`.jvm % testAndCompile,
    )

lazy val `harness-payments` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-payments"))
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
      `harness-email-model` % testAndCompile,
      `harness-pk` % testAndCompile,
    )

lazy val `harness-web` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-web"))
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
      `harness-zio` % testAndCompile,
      `harness-pk` % testAndCompile,
    )

lazy val `harness-http-client` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-http-client"))
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
    .in(file("modules/harness-http-server"))
    .settings(
      name := "harness-http-server",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web`.jvm % testAndCompile,
    )

lazy val `harness-web-ui` =
  project
    .in(file("modules/harness-web-ui"))
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

lazy val `harness-js-plugin` =
  project
    .in(file("modules/harness-js-plugin"))
    .enablePlugins(SbtPlugin)
    .settings(
      name := "harness-js-plugin",
      scalaVersion := "2.12.13",
      addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.2"),
      publishSettings,
      testSettings,
    )

lazy val `harness-web-app-template-plugin` =
  project
    .in(file("modules/harness-web-app-template-plugin"))
    .enablePlugins(SbtPlugin)
    .settings(
      name := "harness-web-app-template-plugin",
      scalaVersion := "2.12.13",
      version := "SNAPSHOT--0.1.12",
      publishSettings,
      testSettings,
    )

// =====| Harness Archive |=====

lazy val `harness-archive` =
  project
    .in(file("harness-archive"))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      // TODO (KR) : inline
      `harness-archive--non-legacy`,
      // TODO (KR) : remove
      `harness-archive--legacy--model`.jvm,
      `harness-archive--legacy--model`.js,
      `harness-archive--legacy--api`,
      `harness-archive--legacy--ui-web`,
      `harness-archive--legacy--client`.jvm,
      `harness-archive--legacy--client`.js,
    )

// TODO (KR) : inline

lazy val `harness-archive--non-legacy` =
  project
    .in(file("harness-archive/modules"))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      `harness-archive--api-model`.jvm,
      `harness-archive--api-model`.js,
      `harness-archive--domain-model`,
      `harness-archive--domain`,
      `harness-archive--db-model`,
      `harness-archive--domain-impl`,
      `harness-archive--web-server`,
      `harness-archive--ui-web`,
    )

lazy val `harness-archive--api-model` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-archive/modules/api-model"))
    .settings(
      name := "harness-archive--api-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web` % testAndCompile,
      `harness-email-model` % testAndCompile,
      `harness-pk` % testAndCompile,
      `harness-payments` % testAndCompile,
      `harness-zio-test` % Test,
    )

lazy val `harness-archive--domain-model` =
  project
    .in(file("harness-archive/modules/domain-model"))
    .settings(
      name := "harness-archive--domain-model",
      publish / skip := true,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % "0.4",
      ),
    )
    .dependsOn(
      `harness-archive--api-model`.jvm % testAndCompile,
    )

lazy val `harness-archive--domain` =
  project
    .in(file("harness-archive/modules/domain"))
    .settings(
      name := "harness-archive--domain",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-archive--domain-model` % testAndCompile,
    )

lazy val `harness-archive--db-model` =
  project
    .in(file("harness-archive/modules/db-model"))
    .settings(
      name := "harness-archive--db-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-archive--domain-model` % testAndCompile,
      `harness-sql` % testAndCompile,
    )

lazy val `harness-archive--domain-impl` =
  project
    .in(file("harness-archive/modules/domain-impl"))
    .settings(
      name := "harness-archive--domain-impl",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-archive--domain` % testAndCompile,
      `harness-archive--db-model` % testAndCompile,
      `harness-email` % testAndCompile,
    )

lazy val `harness-archive--web-server` =
  project
    .in(file("harness-archive/modules/web-server"))
    .settings(
      name := "harness-archive--web-server",
      publish / skip := true,
      miscSettings,
      testSettings,
      assemblyJarName := {
        val appVersion = scala.sys.env.get("WEB_SERVER_VERSION")
        val versionSuffix = appVersion.fold("")(v => s"--$v")
        s"../artifacts/${name.value}$versionSuffix.jar"
      },
    )
    .dependsOn(
      `harness-archive--domain-impl` % testAndCompile,
      `harness-http-server` % testAndCompile,
    )

lazy val `harness-archive--ui-web` =
  project
    .in(file("harness-archive/modules/ui-web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "harness-archive--ui-web",
      publish / skip := true,
      webCompDirs := Seq(
        file("harness-archive/modules/web-server/src/main/resources/res/js"),
        file("harness-archive/res/js"),
      ),
      miscSettings,
      testSettings,
      scalaJSUseMainModuleInitializer := true,
    )
    .dependsOn(
      `harness-archive--api-model`.js % testAndCompile,
      `harness-web-ui` % testAndCompile,
    )

// TODO (KR) : remove legacy

lazy val `harness-archive--legacy--client` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-archive/client"))
    .settings(
      name := "harness-archive--legacy--client",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-http-client` % testAndCompile,
      `harness-archive--legacy--model` % testAndCompile,
    )

lazy val `harness-archive--legacy--model` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-archive/model"))
    .settings(
      name := "harness-archive--legacy--model",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web` % testAndCompile,
      `harness-email-model` % testAndCompile,
    )

lazy val `harness-archive--legacy--db-model` =
  project
    .in(file("harness-archive/db-model"))
    .settings(
      name := "harness-archive--legacy--db-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-sql` % testAndCompile,
      `harness-archive--legacy--model`.jvm % testAndCompile,
    )

lazy val `harness-archive--legacy--api` =
  project
    .in(file("harness-archive/api"))
    .settings(
      name := "harness-archive--legacy--api",
      publish / skip := true,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % "0.4",
        MyOrg %% "slyce-parse" % "2.0.9",
      ),
    )
    .dependsOn(
      `harness-archive--legacy--model`.jvm % testAndCompile,
      `harness-archive--legacy--db-model` % testAndCompile,
      `harness-http-server` % testAndCompile,
      `harness-email` % testAndCompile,
    )

lazy val `harness-archive--legacy--ui-web` =
  project
    .in(file("harness-archive/ui-web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "harness-archive--legacy--ui-web",
      publish / skip := true,
      webCompDirs := Seq(
        file("harness-archive/api/src/main/resources/res/js"),
        file("harness-archive/res/js"),
      ),
      miscSettings,
      testSettings,
      scalaJSUseMainModuleInitializer := true,
    )
    .dependsOn(
      `harness-archive--legacy--model`.js % testAndCompile,
      `harness-web-ui` % testAndCompile,
    )

// =====| Harness Web App Template |=====

lazy val `harness-web-app-template` =
  project
    .in(file("harness-web-app-template/modules"))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      `harness-web-app-template--api-model`.jvm,
      `harness-web-app-template--api-model`.js,
      `harness-web-app-template--domain-model`,
      `harness-web-app-template--domain`,
      `harness-web-app-template--db-model`,
      `harness-web-app-template--domain-impl`,
      `harness-web-app-template--web-server`,
      `harness-web-app-template--ui-web`,
    )

lazy val `harness-web-app-template--api-model` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-web-app-template/modules/api-model"))
    .settings(
      name := "harness-web-app-template--api-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web` % testAndCompile,
      `harness-email-model` % testAndCompile,
      `harness-pk` % testAndCompile,
      `harness-payments` % testAndCompile,
      `harness-zio-test` % Test,
    )

lazy val `harness-web-app-template--domain-model` =
  project
    .in(file("harness-web-app-template/modules/domain-model"))
    .settings(
      name := "harness-web-app-template--domain-model",
      publish / skip := true,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % "0.4",
      ),
    )
    .dependsOn(
      `harness-web-app-template--api-model`.jvm % testAndCompile,
    )

lazy val `harness-web-app-template--domain` =
  project
    .in(file("harness-web-app-template/modules/domain"))
    .settings(
      name := "harness-web-app-template--domain",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web-app-template--domain-model` % testAndCompile,
    )

lazy val `harness-web-app-template--db-model` =
  project
    .in(file("harness-web-app-template/modules/db-model"))
    .settings(
      name := "harness-web-app-template--db-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web-app-template--domain-model` % testAndCompile,
      `harness-sql` % testAndCompile,
    )

lazy val `harness-web-app-template--domain-impl` =
  project
    .in(file("harness-web-app-template/modules/domain-impl"))
    .settings(
      name := "harness-web-app-template--domain-impl",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web-app-template--domain` % testAndCompile,
      `harness-web-app-template--db-model` % testAndCompile,
      `harness-email` % testAndCompile,
    )

lazy val `harness-web-app-template--web-server` =
  project
    .in(file("harness-web-app-template/modules/web-server"))
    .settings(
      name := "harness-web-app-template--web-server",
      publish / skip := true,
      miscSettings,
      testSettings,
      assemblyJarName := {
        val appVersion = scala.sys.env.get("WEB_SERVER_VERSION")
        val versionSuffix = appVersion.fold("")(v => s"--$v")
        s"../artifacts/${name.value}$versionSuffix.jar"
      },
    )
    .dependsOn(
      `harness-web-app-template--domain-impl` % testAndCompile,
      `harness-http-server` % testAndCompile,
    )

lazy val `harness-web-app-template--ui-web` =
  project
    .in(file("harness-web-app-template/modules/ui-web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "harness-web-app-template--ui-web",
      publish / skip := true,
      webCompDirs := Seq(
        file("harness-web-app-template/modules/web-server/src/main/resources/res/js"),
        file("harness-web-app-template/res/js"),
      ),
      miscSettings,
      testSettings,
      scalaJSUseMainModuleInitializer := true,
    )
    .dependsOn(
      `harness-web-app-template--api-model`.js % testAndCompile,
      `harness-web-ui` % testAndCompile,
    )
