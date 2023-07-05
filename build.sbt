//

// =====| Shared Settings |=====

enablePlugins(GitVersioning)
git.gitTagToVersionNumber := { tag =>
  if (tag.matches("^\\d+\\..*$")) Some(tag)
  else None
}

val Scala_3 = "3.2.2"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "harness"

ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

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
      `harness-test`.js,
      `harness-test`.jvm,
      `harness-core`.js,
      `harness-core`.jvm,
      `harness-csv`.js,
      `harness-csv`.jvm,
      `harness-xml`,
      `harness-cli`.js,
      `harness-cli`.jvm,
      `harness-zio`.js,
      `harness-zio`.jvm,
      `harness-sql`,
      `harness-web`.js,
      `harness-web`.jvm,
      `harness-http-client`.js,
      `harness-http-client`.jvm,
      `harness-http-server`,
      `harness-http-server-test`,
      `harness-web-ui`,
      `harness-web-app-template`,
      `harness-archive`,
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
      sonatypeCredentialHost := "s01.oss.sonatype.org",
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
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`harness-core` % "test->test;compile->compile")

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
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .dependsOn(`harness-core`.jvm % "test->test;compile->compile")

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
        "dev.zio" %%% "zio-json" % "0.3.0",
      ),
    )
    .dependsOn(`harness-cli` % "test->test;compile->compile")

lazy val `harness-sql` =
  project
    .in(file("harness-sql"))
    .settings(
      name := "harness-sql",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %% "shapeless3-deriving" % "3.0.1",
        "org.postgresql" % "postgresql" % "42.5.0",
      ),
      Test / fork := true,
    )
    .dependsOn(`harness-zio`.jvm % "test->test;compile->compile")

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
    .dependsOn(`harness-zio` % "test->test;compile->compile")

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
    .dependsOn(`harness-web` % "test->test;compile->compile")

lazy val `harness-http-server` =
  project
    .in(file("harness-http-server"))
    .settings(
      name := "harness-http-server",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`harness-web`.jvm % "test->test;compile->compile")

lazy val `harness-http-server-test` =
  project
    .in(file("harness-http-server-test"))
    .settings(
      name := "harness-http-server-test",
      publishSettings,
      miscSettings,
    )
    .dependsOn(
      `harness-test`.jvm,
      `harness-http-server`,
      `harness-sql`,
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
    .dependsOn(`harness-http-client`.js % "test->test;compile->compile")

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
    .dependsOn(`harness-http-client`, `harness-archive-model`)

lazy val `harness-archive-model` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-archive/model"))
    .settings(
      name := "harness-archive-model",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`harness-web`)

lazy val `harness-archive-db-model` =
  project
    .in(file("harness-archive/db-model"))
    .settings(
      name := "harness-archive-db-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(`harness-sql`, `harness-archive-model`.jvm)

lazy val `harness-archive-api` =
  project
    .in(file("harness-archive/api"))
    .settings(
      name := "harness-archive-api",
      publish / skip := true,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % "0.4",
      ),
    )
    .dependsOn(`harness-archive-model`.jvm, `harness-archive-db-model`, `harness-http-server`, `harness-http-server-test` % Test)

lazy val `harness-archive-ui-web` =
  project
    .in(file("harness-archive/ui-web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "harness-archive-ui-web",
      publish / skip := true,
      miscSettings,
      testSettings,
      scalaJSUseMainModuleInitializer := true,
      buildUI :=
        Def.inputTaskDyn {
          import complete.DefaultParsers._

          val resDir = file("/home/kalin/dev/current/harness/harness-archive/res")

          lazy val fast = (fastLinkJS, "fastopt")
          lazy val full = (fullLinkJS, "opt")

          val args: List[String] = spaceDelimited("<arg>").parsed.toList
          val (t, s) =
            if (args.contains("-F")) full
            else fast
          val m = !args.contains("-m")

          Def.sequential(
            Def.inputTask { println("Running 'webComp'...") }.toTask(""),
            Compile / t,
            Def
              .inputTask {
                def jsFile(fName: String): File = {
                  val crossTargetDir = (crossTarget in (Compile / t)).value
                  val projectName = normalizedName.value
                  file(s"$crossTargetDir/$projectName-$s/$fName")
                }

                val moveToDir = resDir / "js"

                val files =
                  jsFile("main.js") ::
                    (if (m) jsFile("main.js.map") :: Nil else Nil)

                moveToDir.mkdirs()
                moveToDir.listFiles.foreach { f =>
                  if (f.name.contains("main.js"))
                    f.delete()
                }
                files.foreach { f =>
                  IO.copyFile(f, new File(moveToDir, f.getName))
                }

                ()
              }
              .toTask(""),
          )
        }.evaluated,
    )
    .dependsOn(`harness-archive-model`.js, `harness-web-ui`)

// =====| Harness Web App Template |=====

lazy val `harness-web-app-template` =
  project
    .in(file("harness-web-app-template"))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      `harness-web-app-template--model`.jvm,
      `harness-web-app-template--model`.js,
      `harness-web-app-template--api`,
      `harness-web-app-template--ui-web`,
    )

lazy val `harness-web-app-template--model` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("harness-web-app-template/model"))
    .settings(
      name := "harness-web-app-template--model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(`harness-web`)

lazy val `harness-web-app-template--db-model` =
  project
    .in(file("harness-web-app-template/db-model"))
    .settings(
      name := "harness-web-app-template--db-model",
      publish / skip := true,
      miscSettings,
      testSettings,
    )
    .dependsOn(`harness-sql`)

lazy val `harness-web-app-template--api` =
  project
    .in(file("harness-web-app-template/api"))
    .settings(
      name := "harness-web-app-template--api",
      publish / skip := true,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % "0.4",
      ),
    )
    .dependsOn(`harness-web-app-template--model`.jvm, `harness-web-app-template--db-model`, `harness-http-server`, `harness-http-server-test` % Test)

lazy val buildUI: InputKey[Unit] = inputKey("build UI")

lazy val `harness-web-app-template--ui-web` =
  project
    .in(file("harness-web-app-template/ui-web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "harness-web-app-template--ui-web",
      publish / skip := true,
      miscSettings,
      testSettings,
      scalaJSUseMainModuleInitializer := true,
      buildUI :=
        Def.inputTaskDyn {
          import complete.DefaultParsers._

          val resDir = file("/home/kalin/dev/current/harness/harness-web-app-template/res")

          lazy val fast = (fastLinkJS, "fastopt")
          lazy val full = (fullLinkJS, "opt")

          val args: List[String] = spaceDelimited("<arg>").parsed.toList
          val (t, s) =
            if (args.contains("-F")) full
            else fast
          val m = !args.contains("-m")

          Def.sequential(
            Def.inputTask { println("Running 'webComp'...") }.toTask(""),
            Compile / t,
            Def
              .inputTask {
                def jsFile(fName: String): File = {
                  val crossTargetDir = (crossTarget in (Compile / t)).value
                  val projectName = normalizedName.value
                  file(s"$crossTargetDir/$projectName-$s/$fName")
                }

                val moveToDir = resDir / "js"

                val files =
                  jsFile("main.js") ::
                    (if (m) jsFile("main.js.map") :: Nil else Nil)

                moveToDir.mkdirs()
                moveToDir.listFiles.foreach { f =>
                  if (f.name.contains("main.js"))
                    f.delete()
                }
                files.foreach { f =>
                  IO.copyFile(f, new File(moveToDir, f.getName))
                }

                ()
              }
              .toTask(""),
          )
        }.evaluated,
    )
    .dependsOn(`harness-web-app-template--model`.js, `harness-web-ui`)
