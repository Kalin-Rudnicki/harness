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
      // `harness-archive`,
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
      // General
      `harness-console`,
      `harness-core`.jvm,
      `harness-core`.native,
      `harness-core`.js,
      `harness-deriving`.jvm,
      `harness-deriving`.native,
      `harness-deriving`.js,
      `harness-pk`.jvm,
      `harness-pk`.native,
      `harness-pk`.js,
      `harness-schema`.jvm,
      `harness-schema`.native,
      `harness-schema`.js,
      `harness-zio`.jvm,
      `harness-zio`.native,
      `harness-zio`.js,
      `harness-zio-json`.jvm,
      `harness-zio-json`.native,
      `harness-zio-json`.js,

      // Testing
      `harness-test`.jvm,
      `harness-test`.native,
      `harness-test`.js,
      `harness-test-container`,
      `harness-test-container-postgres`,
      `harness-zio-mock`.jvm,
      `harness-zio-mock`.native,
      `harness-zio-mock`.js,
      `harness-zio-test`.jvm,
      `harness-zio-test`.native,
      `harness-zio-test`.js,

      // Parsing
      `harness-cli`.jvm,
      `harness-cli`.native,
      `harness-cli`.js,
      `harness-csv`.jvm,
      `harness-csv`.native,
      `harness-csv`.js,
      `harness-xml`,

      // Web
      `harness-endpoint`.jvm,
      `harness-endpoint`.js,
      `harness-http-client`.jvm,
      `harness-http-client`.js,
      `harness-http-server`,
      `harness-web`.jvm,
      `harness-web`.js,
      `harness-web-ui`,

      // Plugins
      `harness-js-plugin`,

      // Other
      `harness-email`,
      `harness-email-model`.jvm,
      `harness-email-model`.js,
      `harness-kafka`,
      `harness-payments`.jvm,
      `harness-payments`.js,
      `harness-sql`,
      `harness-sql-mock`,
    )

// =====| General |=====

lazy val `harness-console` =
  project
    .in(file("modules/harness-console"))
    .settings(
      name := "harness-console",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio`.jvm,
      `harness-zio-test`.jvm % Test,
    )

lazy val `harness-core` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-core"))
    .settings(
      name := "harness-core",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % Versions.catsCore,
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`harness-test` % Test)

lazy val `harness-deriving` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-deriving"))
    .settings(
      name := "harness-deriving",
      publishSettings,
      miscSettings,
      testSettings,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(
      `harness-core` % testAndCompile,
      `harness-zio-test` % Test,
    )

lazy val `harness-pk` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-pk"))
    .settings(
      name := "harness-pk",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio-json" % Versions.zioJson,
      ),
    )
    .dependsOn(
      `harness-schema` % testAndCompile,
    )

lazy val `harness-schema` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-schema"))
    .settings(
      name := "harness-schema",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .jvmSettings(
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio-json` % testAndCompile,
      `harness-deriving` % testAndCompile,
    )

lazy val `harness-zio` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-zio"))
    .settings(
      name := "harness-zio",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio" % Versions.zio,
      ),
    )
    .jvmSettings(
      Test / fork := true,
    )
    .dependsOn(
      `harness-cli` % testAndCompile,
      `harness-zio-json` % testAndCompile,
    )

lazy val `harness-zio-json` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-zio-json"))
    .settings(
      name := "harness-zio-json",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "dev.zio" %%% "zio-json" % Versions.zioJson,
      ),
    )
    .jvmSettings(
      Test / fork := true,
    )
    .dependsOn(
      `harness-core` % testAndCompile,
    )

// =====| Testing |=====

lazy val `harness-test` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-test"))
    .settings(
      name := "harness-test",
      publishSettings,
      miscSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % Versions.catsCore,
        "dev.zio" %%% "zio-test" % Versions.zio,
        "dev.zio" %%% "zio-test-sbt" % Versions.zio,
      ),
    )

lazy val `harness-test-container` =
  project
    .in(file("modules/harness-test-container"))
    .settings(
      name := "harness-test-container",
      publishSettings,
      miscSettings,
    )
    .dependsOn(
      `harness-zio-test`.jvm,
    )

lazy val `harness-test-container-postgres` =
  project
    .in(file("modules/harness-test-container-postgres"))
    .settings(
      name := "harness-test-container-postgres",
      publishSettings,
      miscSettings,
    )
    .dependsOn(
      `harness-test-container`,
      `harness-sql`,
    )

lazy val `harness-zio-mock` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-zio-mock"))
    .settings(
      name := "harness-zio-mock",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % Versions.catsCore,
        "dev.zio" %%% "zio" % Versions.zio,
      ),
    )
    .jvmSettings(
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio-test` % Test,
    )

lazy val `harness-zio-test` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("modules/harness-zio-test"))
    .settings(
      name := "harness-zio-test",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-zio` % testAndCompile,
      `harness-test` % testAndCompile,
    )

// =====| Parsing |=====

lazy val `harness-csv` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
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
        "org.scala-lang.modules" %% "scala-xml" % Versions.scalaXml,
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(
      `harness-core`.jvm % testAndCompile,
    )

lazy val `harness-cli` =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
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

// =====| Web |=====

lazy val `harness-endpoint` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("modules/harness-endpoint"))
    .settings(
      name := "harness-endpoint",
      publishSettings,
      miscSettings,
      testSettings,
      Test / fork := true,
    )
    .dependsOn(
      `harness-web` % testAndCompile,
      `harness-schema` % testAndCompile,
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
      `harness-endpoint` % testAndCompile,
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
      `harness-endpoint`.jvm % testAndCompile,
      `harness-http-client`.jvm % Test,
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
        "com.lihaoyi" %%% "scalatags" % Versions.scalaTags,
        "io.github.cquiroz" %%% "scala-java-time" % Versions.scalaJavaTime,
        "com.github.julien-truffaut" %%% "monocle-core" % Versions.monocle,
        "com.github.julien-truffaut" %%% "monocle-macro" % Versions.monocle,
      ),
    )
    .dependsOn(
      `harness-zio` % testAndCompile,
      `harness-pk` % testAndCompile,
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

// =====| Plugins |=====

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

// =====| Other |=====

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
        "com.sun.mail" % "javax.mail" % Versions.javaMail,
      ),
    )
    .dependsOn(
      `harness-schema` % testAndCompile,
      `harness-zio` % testAndCompile,
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
        "dev.zio" %% "zio-kafka" % Versions.zioKafka,
      ),
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio`.jvm % testAndCompile,
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
        "com.stripe" % "stripe-java" % Versions.stripe,
      ),
    )
    .jsConfigure(_.dependsOn(`harness-web-ui` % testAndCompile))
    .dependsOn(
      `harness-email-model` % testAndCompile,
      `harness-pk` % testAndCompile,
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
        "org.typelevel" %% "shapeless3-deriving" % Versions.shapeless3Deriving,
        "org.postgresql" % "postgresql" % Versions.postgresql,
      ),
      Test / fork := true,
    )
    .dependsOn(
      `harness-zio`.jvm % testAndCompile,
      `harness-pk`.jvm % testAndCompile,
    )

lazy val `harness-sql-mock` =
  project
    .in(file("modules/harness-sql-mock"))
    .settings(
      name := "harness-sql-mock",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "com.github.julien-truffaut" %%% "monocle-core" % Versions.monocle,
        "com.github.julien-truffaut" %%% "monocle-macro" % Versions.monocle,
      ),
      Test / fork := true,
    )
    .dependsOn(`harness-sql` % testAndCompile)

// =====| Harness Archive |=====

lazy val `harness-archive` =
  project
    .in(file("harness-archive"))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      `harness-archive-model`.jvm,
      `harness-archive-model`.js,
      `harness-archive-api`,
      `harness-archive-ui-web`,
      `harness-archive-client`.jvm,
      `harness-archive-client`.js,
    )

lazy val `harness-archive-client` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-archive/client"))
    .settings(
      name := "harness-archive-client",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-http-client` % testAndCompile,
      `harness-archive-model` % testAndCompile,
    )

lazy val `harness-archive-model` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-archive/model"))
    .settings(
      name := "harness-archive-model",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web` % testAndCompile,
      `harness-email-model` % testAndCompile,
    )

lazy val `harness-archive-db-model` =
  project
    .in(file("harness-archive/db-model"))
    .settings(
      name := "harness-archive-db-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-sql` % testAndCompile,
      `harness-archive-model`.jvm % testAndCompile,
    )

lazy val `harness-archive-api` =
  project
    .in(file("harness-archive/api"))
    .settings(
      name := "harness-archive-api",
      publish / skip := true,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % Versions.bcrypt,
        MyOrg %% "slyce-parse" % Versions.slyce,
      ),
    )
    .dependsOn(
      `harness-archive-model`.jvm % testAndCompile,
      `harness-archive-db-model` % testAndCompile,
      `harness-http-server` % testAndCompile,
      `harness-email` % testAndCompile,
    )

lazy val `harness-archive-ui-web` =
  project
    .in(file("harness-archive/ui-web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "harness-archive-ui-web",
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
      `harness-archive-model`.js % testAndCompile,
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
      `harness-web-app-template--api-impl`,
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
      `harness-email-model` % testAndCompile,
      `harness-endpoint` % testAndCompile,
      `harness-payments` % testAndCompile,
      `harness-pk` % testAndCompile,
      `harness-web` % testAndCompile,
      `harness-zio-test` % Test,
    )

lazy val `harness-web-app-template--api` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-web-app-template/modules/api"))
    .settings(
      name := "harness-web-app-template--api",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web-app-template--api-model` % testAndCompile,
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
        "org.mindrot" % "jbcrypt" % Versions.bcrypt,
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
      `harness-sql-mock` % Test,
    )

lazy val `harness-web-app-template--api-impl` =
  project
    .in(file("harness-web-app-template/modules/api-impl"))
    .settings(
      name := "harness-web-app-template--api-impl",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(
      `harness-web-app-template--domain` % testAndCompile,
      `harness-web-app-template--api`.jvm % testAndCompile,
      `harness-sql` % testAndCompile,
      `harness-http-server` % testAndCompile,
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
      `harness-web-app-template--api-impl` % testAndCompile,
      `harness-web-app-template--domain-impl` % testAndCompile,
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
      `harness-web-app-template--api`.js % testAndCompile,
      `harness-web-ui` % testAndCompile,
    )
