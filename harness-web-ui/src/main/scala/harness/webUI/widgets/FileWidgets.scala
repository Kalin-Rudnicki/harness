package harness.webUI.widgets

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import harness.core.*
import harness.webUI.*
import harness.webUI.facades.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.zio.*
import org.scalajs.dom.{File, FileList}
import org.scalajs.dom.DataTransfer
import org.scalajs.dom.DOMList
import scala.scalajs.js
import zio.*

object FileWidgets {

  private def fileListToList(files: FileList): List[File] =
    0.until(files.length)
      .toList
      .map(files(_))

  private def validateSingleFile(files: Option[NonEmptyList[File]]): EitherNel[String, Option[File]] =
    files match {
      case None                          => None.asRight
      case Some(NonEmptyList(file, Nil)) => file.some.asRight
      case Some(_)                       => "Single file expected".leftNel
    }

  private def showFiles(empty: CModifier): Modifier[Option[NonEmptyList[File]]] =
    PModifier.builder.withState[Option[NonEmptyList[File]]] {
      case None                          => empty
      case Some(NonEmptyList(file, Nil)) => file.name
      case Some(files)                   => ul(PModifier.foreach(files.toList)(f => li(f.name)))
    }

  private def genFileInput(
      inputMods: CModifier*,
  ): CNodeWidgetA[Option[NonEmptyList[File]]] =
    label(
      DefaultStyleSheet.fileInput,
      onDragOver := { e =>
        e.preventDefault()
      },
      PModifier.builder.withAction[Option[NonEmptyList[File]]] { rh =>
        onDrop := { e =>
          e.preventDefault()
          rh.raise(Raise.Action(fileListToList(e.dataTransfer.files).toNel))
        }
      },
      input(
        display.none,
        `type`.file,
        inputMods,
        PModifier.builder.withAction[Option[NonEmptyList[File]]] { rh =>
          onChange := { e =>
            val files: List[File] =
              fileListToList {
                e.target
                  .asInstanceOf[js.Dynamic]
                  .files
                  .asInstanceOf[FileList]
              }

            rh.raise(Raise.Action(files.toNel))
          }
        },
      ),
    )

  def singleFileInput(
      labelMod: Modifier[Option[NonEmptyList[File]]] = showFiles("Click or drag to select file"),
  ): ModifierV[Option[NonEmptyList[File]], Option[File]] =
    genFileInput()(labelMod)
      .flatMapActionZ { files =>
        validateSingleFile(files) match {
          case Right(file)  => ZIO.succeed(Raise.setState(file.map(NonEmptyList.one)))
          case Left(errors) => ZIO.fail(HError(errors.map(HError.UserError(_))))
        }
      }
      .eitherAsValue(validateSingleFile)

  def multiFileInput(
      labelMod: Modifier[Option[NonEmptyList[File]]] = showFiles("Click or drag to select files"),
  ): ModifierV[Option[NonEmptyList[File]], Option[NonEmptyList[File]]] =
    genFileInput(multiple.empty)(labelMod)
      .flatMapAction { files => Raise.setState(files) }
      .asValue(identity)

  def singleFileDrop(
      labelMod: CModifier = "Click or drag to select file",
  ): CModifierA[Option[File]] =
    genFileInput()(labelMod).flatMapActionZ { files =>
      validateSingleFile(files) match {
        case Right(file)  => ZIO.succeed(Raise.Action(file))
        case Left(errors) => ZIO.fail(HError(errors.map(HError.UserError(_))))
      }
    }

  def multiFileDrop(
      labelMod: CModifier = "Click or drag to select files",
  ): CModifierA[Option[NonEmptyList[File]]] =
    genFileInput(multiple.empty)(labelMod)

  object dropOnly {

    val widget: CNodeWidgetA[Chunk[(Option[String], File)]] =
      span(
        DefaultStyleSheet.fileInput,
        "Drop files/directories here!",
        onDragOver := { e =>
          e.preventDefault()
        },
        PModifier.builder.withAction[Chunk[(Option[String], File)]] { rh =>
          onDrop := { e =>
            e.preventDefault()
            rh.raiseZIO(
              dropOnly
                .fileHandles(e.dataTransfer)
                .flatMap { fileHandles =>
                  Logger.log.info("Searching for files") *>
                    filesForChunk(fileHandles).runCollect.map(Raise.Action(_))
                },
            )
          }
        },
      )

    private def fileHandles(dataTransfer: DataTransfer): HTask[Chunk[FileSystemHandle]] = {
      val domList: DOMList[js.Dynamic] = dataTransfer.asInstanceOf[js.Dynamic].items.asInstanceOf[DOMList[js.Dynamic]]
      val seq: Seq[js.Dynamic] = DOMList.domListAsSeq(domList).toSeq
      ZIO.foreachPar(Chunk.fromIterable(seq)) { item => ZIO.fromPromiseJS(item.getAsFileSystemHandle().asInstanceOf[js.Promise[FileSystemHandle]]) }
    }.mapError(HError.SystemFailure("DataTransfer -> FileSystemHandle", _))

  }

}
