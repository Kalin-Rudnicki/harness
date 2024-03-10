package harness.webAppTemplatePlugin

import sbt._
import sbt.Keys._
import sbt.io.IO

object HarnessWebAppTemplatePlugin extends AutoPlugin {

  override def trigger = allRequirements

  object autoImport {

    val harnessWebAppUnTemplate: InputKey[Unit] = inputKey("harnessWebAppUnTemplate")

  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      harnessWebAppUnTemplate :=
        Def.inputTaskDyn {
          import complete.DefaultParsers._

          val args: List[String] = spaceDelimited("<arg>").parsed.toList

          Def
            .inputTask {
              println(s"Running 'harnessWebAppUnTemplate' for '${name.value}'")

              def findArg(argName: String): Option[String] = {
                val reg = s"^--$argName=(.*)$$".r
                args.collectFirst { case reg(str) => str }
              }

              def getArg(argName: String): String =
                findArg(argName).getOrElse(throw new RuntimeException(s"Missing arg in format of: --$argName=..."))

              val newPackages = getArg("new-package").split('.').toList.map(_.trim).filter(_.nonEmpty)
              if (newPackages.isEmpty) throw new RuntimeException("new packages is empty")

              val allSources = (Compile / sources).value ++ (Test / sources).value
              val allSourceDirs = (Compile / sourceDirectories).value ++ (Test / sourceDirectories).value

              val oldPackages = findArg("old-package").getOrElse("template").split('.').toList.map(_.trim).filter(_.nonEmpty)
              if (oldPackages.isEmpty) throw new RuntimeException("new packages is empty")
              val importPackageReg = s"(import|package) ${oldPackages.mkString("\\.")}".r

              allSources.foreach { src =>
                if (src.exists) {
                  try {
                    println(s"fixing packages/imports '${src.getPath}'")
                    val fileContents = IO.read(src)
                    val replacedContents = importPackageReg.replaceAllIn(fileContents, m => s"${m.group(1)} ${newPackages.mkString(".")}")
                    IO.write(src, replacedContents)
                  } catch { case _: Throwable => }
                }
              }

              allSourceDirs.foreach { src =>
                if (src.exists) {
                  try {
                    val templateSrc = src / oldPackages.mkString("/")
                    if (templateSrc.exists && templateSrc.isDirectory) {
                      val newSrc = src / newPackages.mkString("/")
                      val newSrcParent = newSrc.getParentFile
                      if (!newSrcParent.exists)
                        newSrcParent.mkdirs

                      println(s"moving '${templateSrc.getPath}' -> '${newSrc.getPath}'")
                      IO.move(templateSrc, newSrc)
                    }
                  } catch { case _: Throwable => }
                }
              }
            }
            .toTask("")
        }.evaluated,
    )

}
