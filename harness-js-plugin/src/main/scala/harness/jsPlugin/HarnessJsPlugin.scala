package harness.jsPlugin

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt._
import sbt.Keys._

object HarnessJsPlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

    val webComp: InputKey[Unit] = inputKey("webComp")
    val webCompDirs = settingKey[Seq[File]]("webCompDirs")

  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      webComp :=
        Def.inputTaskDyn {
          import complete.DefaultParsers._

          val args: List[String] = spaceDelimited("<arg>").parsed.toList
          val (task, taskName) =
            if (args.contains("--full")) (fullLinkJS, "opt")
            else (fastLinkJS, "fastopt")

          val copySourceMap = !args.contains("--no-source-map")

          Def.sequential(
            Def
              .inputTask { println("Running 'webComp'...") }
              .toTask(""),
            Compile / task,
            Def
              .inputTask {
                def jsFile(name: String): File = {
                  val targetDir = (Compile / task / crossTarget).value
                  val projectName = normalizedName.value
                  new File(s"$targetDir/$projectName-$taskName/$name")
                }

                val files =
                  jsFile("main.js") ::
                    (if (copySourceMap) jsFile("main.js.map") :: Nil else Nil)

                webCompDirs.value.foreach { moveToDir =>
                  println(s"copying scripts to '$moveToDir'")

                  moveToDir.mkdirs()
                  moveToDir.listFiles.foreach { f =>
                    if (f.name.contains("main.js"))
                      f.delete()
                  }
                  files.foreach { f =>
                    IO.copyFile(f, new File(moveToDir, f.getName))
                  }
                }

                ()
              }
              .toTask(""),
          )
        }.evaluated,
    )

}
