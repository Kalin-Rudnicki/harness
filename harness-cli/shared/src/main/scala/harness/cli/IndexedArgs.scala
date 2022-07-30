package harness.cli

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import scala.annotation.tailrec

type IndexedArgs = List[Indexed[Arg]]
object IndexedArgs {

  def parse(args: List[String]): Either[String, IndexedArgs] =
    args.traverse(Arg.parse).map {
      _.zipWithIndex.flatMap { (args, idx) =>
        args.map(Indexed(_, idx))
      }
    }

  def parse(args: String*): Either[String, IndexedArgs] =
    IndexedArgs.parse(args.toList)

  def format(args: IndexedArgs): List[String] = {
    @tailrec
    def loop(
        queue: IndexedArgs,
        cache: Option[NonEmptyList[ShortName]],
        stack: List[String],
    ): List[String] =
      queue match {
        case head :: tail =>
          (head.value, cache) match {
            case (Arg.ShortParamMulti(name, _), None)        => loop(tail, NonEmptyList.one(name).some, stack)
            case (Arg.ShortParamMulti(name, _), Some(cache)) => loop(tail, (name :: cache).some, stack)
            case (arg, None)                                 => loop(tail, None, arg.toArgString :: stack)
            case (_, Some(cache))                            => loop(queue, None, cache.toList.reverse.mkString("-", "", "") :: stack)
          }
        case Nil =>
          cache match {
            case Some(cache) => loop(Nil, None, cache.toList.reverse.mkString("-", "", "") :: stack)
            case None        => stack.reverse
          }
      }

    loop(args, None, Nil)
  }

  def remainingInBoth(remainingArgs1: IndexedArgs, remainingArgs2: IndexedArgs): IndexedArgs =
    (remainingArgs1.toSet & remainingArgs2.toSet).toList.sorted(IndexedArgs.ordering)

  val ordering: Ordering[Indexed[Arg]] =
    Ordering.by[Indexed[Arg], Int](_.idx).orElseBy {
      case Indexed(Arg.ShortParamMulti(_, i), _) => i
      case _                                     => 0
    }

}
