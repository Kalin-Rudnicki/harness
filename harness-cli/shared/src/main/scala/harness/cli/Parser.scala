package harness.cli

import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.{StringDecoder, Zip}

trait Parser[T] { self =>

  def buildF(usedParams: Set[Name]): Either[Name, Parser.BuildResult[T]]

  final def finalized: FinalizedParser[T] =
    (Parser.help <^|| self.indentedHelpMessage).buildF(Set.empty) match {
      case Right(buildResult) =>
        FinalizedParser(
          buildResult.usedParams,
          buildResult.helpMessage,
          buildResult.helpExtraMessage,
          buildResult.parse(_) match {
            case Parser.ParseResult.Success(remainingArgs, result) =>
              Parser.errorOnRemainingArgs(remainingArgs) match {
                case None =>
                  result match {
                    case Right(value) => FinalizedParser.Result.Success(value)
                    case Left(false)  => FinalizedParser.Result.Help(false, buildResult.helpMessage)
                    case Left(true)   => FinalizedParser.Result.Help(true, buildResult.helpExtraMessage)
                  }
                case Some(fail) =>
                  FinalizedParser.Result.ParseFail(fail)
              }
            case Parser.ParseResult.Fail(remainingArgs, fail1) =>
              Parser.errorOnRemainingArgs(remainingArgs) match {
                case None        => FinalizedParser.Result.ParseFail(fail1)
                case Some(fail2) => FinalizedParser.Result.ParseFail(ParsingFailure.and(fail1, fail2))
              }
          },
        )
      case Left(duplicateParam) =>
        FinalizedParser(
          Set(duplicateParam),
          HelpMessage.duplicateParam(duplicateParam),
          HelpMessage.duplicateParam(duplicateParam),
          _ => FinalizedParser.Result.BuildFail(duplicateParam),
        )
    }

  // =====| Transforms |=====

  final def map[T2](f: T => T2): Parser[T2] =
    buildF(_).map(_.map(f))

  final def as[T2](f: => T2): Parser[T2] = self.map { _ => f }

  // =====| Combinators |=====

  /**
    * Requires both `self` and `other` to properly parse.<br>
    * The result is the result of `self` and `other` zipped together.
    */
  final def &&[T2](other: Parser[T2])(implicit zip: Zip[T, T2]): Parser[zip.Out] = { usedParams =>
    for {
      buildRes1 <- self.buildF(usedParams)
      buildRes2 <- other.buildF(buildRes1.usedParams)
    } yield Parser.BuildResult(
      buildRes2.usedParams,
      HelpMessage(buildRes1.helpMessage, buildRes2.helpMessage),
      HelpMessage(buildRes1.helpExtraMessage, buildRes2.helpExtraMessage),
      { args =>
        buildRes1.parse(args) match {
          case Parser.ParseResult.Success(args1, result1) =>
            buildRes2.parse(args1) match {
              case Parser.ParseResult.Success(args2, result2) => Parser.ParseResult.Success(args2, zip.zip(result1, result2))
              case fail: Parser.ParseResult.Fail              => fail
            }
          case Parser.ParseResult.Fail(args1, fail1) =>
            buildRes2.parse(args1) match {
              case Parser.ParseResult.Success(args2, _)  => Parser.ParseResult.Fail(args2, fail1)
              case Parser.ParseResult.Fail(args2, fail2) => Parser.ParseResult.Fail(args2, ParsingFailure.and(fail1, fail2))
            }
        }
      },
    )
  }

  /**
    * Attempts to parse `self`.<br>
    * If `self` fails, attempts to parse `other`.<br>
    * If `other` also fails, returns the errors from both.<br>
    * NOTE : It is recommended to call `.indentedHelpMessage` on each parser in a chain of ors.
    */
  final def <||[T2 >: T](other: Parser[T2]): Parser[T2] = { usedParams =>
    for {
      buildRes1 <- self.buildF(usedParams)
      buildRes2 <- other.buildF(usedParams)
    } yield Parser.BuildResult(
      buildRes1.usedParams | buildRes2.usedParams, // TODO (KR) : Should (a <|| b) be allowed to share param names?
      HelpMessage(buildRes1.helpMessage, HelpMessage.Text("OR" :: Nil, Nil), buildRes2.helpMessage),
      HelpMessage(buildRes1.helpExtraMessage, HelpMessage.Text("OR" :: Nil, Nil), buildRes2.helpExtraMessage),
      { args =>
        buildRes1.parse(args) match {
          case success: Parser.ParseResult.Success[T] => success
          case Parser.ParseResult.Fail(remainingArgs1, fail1) =>
            buildRes2.parse(args) match {
              case success: Parser.ParseResult.Success[T2] => success
              case Parser.ParseResult.Fail(remainingArgs2, fail2) =>
                Parser.ParseResult.Fail(
                  IndexedArgs.remainingInBoth(remainingArgs1, remainingArgs2),
                  ParsingFailure.or(fail1, fail2),
                )
            }
        }
      },
    )
  }

  /**
    * Helper for [[<||]], that maps `self` to `Left`, and `other` to `Right`.<br>
    * NOTE : It is recommended to call `.indentedHelpMessage` on each parser in a chain of ors.
    */
  final def <^||[T2](other: Parser[T2]): Parser[Either[T, T2]] =
    self.map(Left(_)) <|| other.map(Right(_))

  /**
    * Attempts to parse `self`.<br>
    * If `self` passes, disregard the value in `self`, and attempt to parse `other`.<br>
    */
  final def ##>[T2](other: Parser[T2]): Parser[T2] = { usedParams =>
    for {
      buildRes1 <- self.buildF(usedParams)
      buildRes2 <- other.buildF(buildRes1.usedParams)
    } yield Parser.BuildResult(
      buildRes2.usedParams,
      HelpMessage(buildRes1.helpMessage, buildRes2.helpExtraMessage),
      HelpMessage(buildRes1.helpExtraMessage, buildRes2.helpExtraMessage),
      buildRes1.parse(_).flatMap { (args, _) => buildRes2.parse(args) },
    )
  }

  /**
    * Attempts to parse `self`.<br>
    * If `self` passes, map to `Some(_)`.<br>
    * If `self` fails with only missing params, pass with `None`.<br>
    * Otherwise, fail.
    */
  final def optional: Parser[Option[T]] =
    self.buildF(_).map { buildResult =>
      Parser
        .BuildResult(
          buildResult.usedParams,
          buildResult.helpMessage,
          HelpMessage.optional(buildResult.helpExtraMessage),
          buildResult.parse,
        )
        .mapArgsAndResult {
          case (_, Parser.ParseResult.Success(remainingArgs, result)) => Parser.ParseResult.Success(remainingArgs, result.some)
          case (args, failResult @ Parser.ParseResult.Fail(remainingArgs, fail)) =>
            if (ParsingFailure.containsOnlyMissingParam(fail) && args.size == remainingArgs.size) Parser.ParseResult.Success(args, None)
            else failResult
        }
    }

  /**
    * Attempts to parse `self`.<br>
    * If `self` passes, pass with that value.<br>
    * If `self` fails with only missing params, pass with `dflt`.<br>
    * Otherwise, fail.
    */
  final def default[T2 >: T](dflt: => T2): Parser[T2] =
    self.buildF(_).map { buildResult =>
      Parser
        .BuildResult(
          buildResult.usedParams,
          buildResult.helpMessage,
          HelpMessage.optional(buildResult.helpExtraMessage),
          buildResult.parse,
        )
        .mapArgsAndResult {
          case (_, success: Parser.ParseResult.Success[T]) => success
          case (args, failResult @ Parser.ParseResult.Fail(remainingArgs, fail)) =>
            if (ParsingFailure.containsOnlyMissingParam(fail) && args.size == remainingArgs.size) Parser.ParseResult.Success(remainingArgs, dflt)
            else failResult
        }
    }

  final def dropAllRemainingArgsIfSuccessful: Parser[T] =
    self.buildF(_).map {
      _.mapResult {
        case Parser.ParseResult.Success(_, result) => Parser.ParseResult.Success(Nil, result)
        case fail: Parser.ParseResult.Fail         => fail
      }
    }

  // =====| HelpMessage |=====

  final def indentedHelpMessage: Parser[T] =
    self.buildF(_).map { buildResult =>
      Parser.BuildResult(
        buildResult.usedParams,
        HelpMessage.Indent(buildResult.helpMessage),
        HelpMessage.Indent(buildResult.helpExtraMessage),
        buildResult.parse,
      )
    }

  final def sectionHelpMessage(section: String): Parser[T] =
    self.buildF(_).map { buildResult =>
      Parser.BuildResult(
        buildResult.usedParams,
        HelpMessage(HelpMessage.Text(section :: Nil, Nil), HelpMessage.Indent(buildResult.helpMessage)),
        HelpMessage(HelpMessage.Text(section :: Nil, Nil), HelpMessage.Indent(buildResult.helpExtraMessage)),
        buildResult.parse,
      )
    }
  
}
object Parser {

  // =====| Types |=====

  final case class BuildResult[+T](
      usedParams: Set[Name],
      helpMessage: HelpMessage,
      helpExtraMessage: HelpMessage,
      parse: IndexedArgs => ParseResult[T],
  ) {

    def mapResult[T2](f: Parser.ParseResult[T] => Parser.ParseResult[T2]): BuildResult[T2] =
      BuildResult(
        usedParams,
        helpMessage,
        helpExtraMessage,
        args => f(parse(args)),
      )

    def mapArgsAndResult[T2](f: (IndexedArgs, Parser.ParseResult[T]) => Parser.ParseResult[T2]): BuildResult[T2] =
      BuildResult(
        usedParams,
        helpMessage,
        helpExtraMessage,
        args => f(args, parse(args)),
      )

    def map[T2](f: T => T2): BuildResult[T2] =
      mapResult(_.map(f))

  }

  sealed trait ParseResult[+T] {

    final def map[T2](f: T => T2): ParseResult[T2] =
      this match {
        case ParseResult.Success(remainingArgs, result) => ParseResult.Success(remainingArgs, f(result))
        case fail: ParseResult.Fail                     => fail
      }

    final def flatMap[T2](f: (IndexedArgs, T) => ParseResult[T2]): ParseResult[T2] =
      this match {
        case ParseResult.Success(remainingArgs, result) => f(remainingArgs, result)
        case fail: ParseResult.Fail                     => fail
      }

  }
  object ParseResult {
    final case class Success[+T](remainingArgs: IndexedArgs, result: T) extends ParseResult[T]
    final case class Fail(remainingArgs: IndexedArgs, fail: ParsingFailure) extends ParseResult[Nothing]
  }

  // =====| Builders |=====

  /**
    * true : help-extra
    * false : help
    */
  val help: Parser[Boolean] =
    (
      Parser
        .present(
          LongName.unsafe("help-extra"),
          true,
          Defaultable.Some(ShortName.unsafe('H')),
          "Show detailed help message" :: Nil,
        )
        .indentedHelpMessage <||
        Parser
          .present(
            LongName.unsafe("help"),
            false,
            Defaultable.Some(ShortName.unsafe('h')),
            "Show help message" :: Nil,
          )
          .indentedHelpMessage
    ).dropAllRemainingArgsIfSuccessful

  def const[T](value: => T): Parser[T] = { usedParams =>
    Parser
      .BuildResult(
        usedParams,
        HelpMessage(),
        HelpMessage(),
        { args =>
          ParseResult.Success(
            args,
            value,
          )
        },
      )
      .asRight
  }

  val unit: Parser[Unit] = Parser.const { () }

  /**
    * Parses a value from a param:
    *   - --base-name VALUE
    *   - --base-name=VALUE
    */
  def value[T](
      baseName: LongName,
      shortParam: Defaultable.Optional[ShortName] = Defaultable.Auto,
      helpHint: List[String] = Nil,
      helpExtraHint: List[String] = Nil,
  )(implicit decoder: StringDecoder[T]): Parser[T] =
    verifyAndDerive.value(_, Param.LongWithValue(baseName), shortParam.map(Param.ShortWithValue(_))).map { (usedParams, long, short) =>
      short match {
        case Some(short) =>
          Parser.BuildResult(
            usedParams,
            HelpMessage.Text(
              s"${long.formattedName}, ${short.formattedName}" :: Nil,
              helpHint,
            ),
            HelpMessage.Text(
              s"${long.formattedName}, ${short.formattedName}" :: Nil,
              helpHint ::: helpExtraHint,
            ),
            (FindFunction.forParam(long) || FindFunction.forParam(short)).toParseFunction(long)(_).flatMap { (args, str) =>
              decoder.decode(str) match {
                case Right(value) => ParseResult.Success(args, value)
                case Left(msg)    => ParseResult.Fail(args, ParsingFailure.MalformedValue(long, str, msg))
              }
            },
          )
        case None =>
          Parser.BuildResult(
            usedParams,
            HelpMessage.Text(
              long.formattedName :: Nil,
              helpHint,
            ),
            HelpMessage.Text(
              long.formattedName :: Nil,
              helpHint ::: helpExtraHint,
            ),
            FindFunction.forParam(long).toParseFunction(long)(_).flatMap { (args, str) =>
              decoder.decode(str) match {
                case Right(value) => ParseResult.Success(args, value)
                case Left(msg)    => ParseResult.Fail(args, ParsingFailure.MalformedValue(long, str, msg))
              }
            },
          )
      }
    }

  // TODO (KR) : more options for lists?
  //           : list -i=1,2,3
  //           : list -i=1 -i=2 -i=3
  //           : list -i=1,2,3 -i=4,5,6

  /**
    * Parses a boolean based on prefix of baseName:
    *   - --[dont-]run : --dont-run [false] / --run [true]
    *   - --[do/dont]-run : --do-run [true] / --dont-run [false]
    */
  object toggle {

    def apply(
        longParam: Param.LongToggle,
        shortParam: Defaultable.Optional[Param.ShortToggle] = Defaultable.Auto,
        helpHint: List[String] = Nil,
        helpExtraHint: List[String] = Nil,
    ): Parser[Boolean] =
      verifyAndDerive.toggle(_, longParam, shortParam).map { case (usedParams, long, short) =>
        short match {
          case Some(short) =>
            BuildResult(
              usedParams,
              HelpMessage.Text(
                s"${long.formattedName}, ${short.formattedName}" :: Nil,
                helpHint,
              ),
              HelpMessage.Text(
                s"${long.formattedName}, ${short.formattedName}" :: Nil,
                helpHint ::: helpExtraHint,
              ),
              (FindFunction.forParam(long) || FindFunction.forParam(short)).toParseFunction(longParam),
            )
          case None =>
            BuildResult(
              usedParams,
              HelpMessage.Text(
                long.formattedName :: Nil,
                helpHint,
              ),
              HelpMessage.Text(
                long.formattedName :: Nil,
                helpHint ::: helpExtraHint,
              ),
              FindFunction.forParam(long).toParseFunction(longParam),
            )
        }
      }

    def prefixTrue(
        truePrefix: LongName,
        baseName: LongName,
        shortParam: Defaultable.Optional[Param.ShortToggle] = Defaultable.Auto,
        helpHint: List[String] = Nil,
        helpExtraHint: List[String] = Nil,
    ): Parser[Boolean] =
      toggle(Param.LongToggle.PrefixTrue(truePrefix, baseName), shortParam, helpHint, helpExtraHint)

    def prefixFalse(
        falsePrefix: LongName,
        baseName: LongName,
        shortParam: Defaultable.Optional[Param.ShortToggle] = Defaultable.Auto,
        helpHint: List[String] = Nil,
        helpExtraHint: List[String] = Nil,
    ): Parser[Boolean] =
      toggle(Param.LongToggle.PrefixFalse(falsePrefix, baseName), shortParam, helpHint, helpExtraHint)

    def prefixBoth(
        truePrefix: LongName,
        falsePrefix: LongName,
        baseName: LongName,
        shortParam: Defaultable.Optional[Param.ShortToggle] = Defaultable.Auto,
        helpHint: List[String] = Nil,
        helpExtraHint: List[String] = Nil,
    ): Parser[Boolean] =
      toggle(Param.LongToggle.PrefixBoth(truePrefix, falsePrefix, baseName), shortParam, helpHint, helpExtraHint)

  }

  /**
    * Parses a boolean based on whether baseName is provided:
    *   - --base-name [ifPresent] / `no param` [!ifPresent]
    */
  def flag(
      baseName: LongName,
      ifPresent: => Boolean = true,
      shortParam: Defaultable.Optional[ShortName] = Defaultable.Auto,
  ): Parser[Boolean] =
    present(
      baseName,
      ifPresent,
      shortParam,
    ).default(!ifPresent)

  /**
    * If present, returns `ifPresent`, otherwise, fails.
    */
  def present[T](
      baseName: LongName,
      ifPresent: => T,
      shortParam: Defaultable.Optional[ShortName] = Defaultable.Auto,
      helpHint: List[String] = Nil,
      helpExtraHint: List[String] = Nil,
  ): Parser[T] =
    verifyAndDerive.flag(_, Param.LongFlag(baseName), shortParam.map(Param.ShortFlag(_))).map { case (usedParams, long, short) =>
      short match {
        case Some(short) =>
          BuildResult(
            usedParams,
            HelpMessage.Text(
              s"${long.formattedName}, ${short.formattedName}" :: Nil,
              helpHint,
            ),
            HelpMessage.Text(
              s"${long.formattedName}, ${short.formattedName}" :: Nil,
              helpHint ::: helpExtraHint,
            ),
            (FindFunction.forParam(long).as {
              ifPresent
            } || FindFunction.forParam(short).as {
              ifPresent
            }).toParseFunction(long),
          )
        case None =>
          BuildResult(
            usedParams,
            HelpMessage.Text(
              long.formattedName :: Nil,
              helpHint,
            ),
            HelpMessage.Text(
              long.formattedName :: Nil,
              helpHint ::: helpExtraHint,
            ),
            FindFunction
              .forParam(long)
              .as {
                ifPresent
              }
              .toParseFunction(long),
          )
      }
    }

  /**
    * Parses a single normal value, without a param.<br>
    * Note that this should be used AFTER all param-based calls, because otherwise, something like:<br>
    * --key KEY-VALUE VALUE<br>
    * would pull out the KEY-VALUE, and then parse --key VALUE
    */
  def rawValue[T](
      baseName: LongName,
      helpHint: List[String] = Nil,
      helpExtraHint: List[String] = Nil,
  )(implicit decoder: StringDecoder[T]): Parser[T] =
    verify.long(_, baseName).map { usedParams =>
      val long = Param.Value(baseName, false)
      BuildResult(
        usedParams,
        HelpMessage.Text(
          s"[$baseName]" :: Nil,
          helpHint,
        ),
        HelpMessage.Text(
          s"[$baseName]" :: Nil,
          helpHint ::: helpExtraHint,
        ),
        FindFunction.forValue.toParseFunction(long)(_).flatMap { (args, value) =>
          decoder.decode(value) match {
            case Right(value) => ParseResult.Success(args, value)
            case Left(msg)    => ParseResult.Fail(args, ParsingFailure.MalformedValue(long, value, msg))
          }
        },
      )
    }

  /**
    * Parses all normal values, without a param.<br>
    * Note that this should be used AFTER all param-based calls, because otherwise, something like:<br>
    * --key KEY-VALUE VALUE<br>
    * would pull out the KEY-VALUE and VALUE, and then parse --key
    */
  def rawValues[T](
      baseName: LongName,
      helpHint: List[String] = Nil,
      helpExtraHint: List[String] = Nil,
  )(implicit decoder: StringDecoder[T]): Parser[List[T]] =
    verify.long(_, baseName).map { usedParams =>
      val long = Param.Value(baseName, true)
      BuildResult(
        usedParams,
        HelpMessage.Text(
          s"[$baseName...]" :: Nil,
          helpHint,
        ),
        HelpMessage.Text(
          s"[$baseName...]" :: Nil,
          helpHint ::: helpExtraHint,
        ),
        { args =>
          val (values, remainingArgs) = args.partitionMap {
            case Indexed(value: Arg.Value, _) => value.toArgString.asLeft
            case arg                          => arg.asRight
          }

          values.traverse { value =>
            decoder.decode(value).leftMap(ParsingFailure.MalformedValue(long, value, _))
          } match {
            case Right(value) => ParseResult.Success(remainingArgs, value)
            case Left(fail)   => ParseResult.Fail(remainingArgs, fail)
          }
        },
      )
    }

  def firstOf[T](parser0: Parser[T], parser1: Parser[T], parserN: Parser[T]*): Parser[T] =
    (parser1 :: parserN.toList).foldLeft(parser0.indentedHelpMessage) { (l, r) => l <|| r.indentedHelpMessage }

  def helpMessage(left: List[String], right: List[String], rightExtra: List[String]): Parser[Unit] = { usedParams =>
    Parser
      .BuildResult(
        usedParams,
        HelpMessage.Text(left, right),
        HelpMessage.Text(left, right ::: rightExtra),
        { args =>
          ParseResult.Success(
            args,
            (),
          )
        },
      )
      .asRight
  }

  // =====| Helpers |=====

  private def errorOnRemainingArgs(args: IndexedArgs): Option[ParsingFailure] =
    args.map(ParsingFailure.UnparsedArg(_)) match {
      case Nil          => None
      case head :: Nil  => head.some
      case head :: tail => ParsingFailure.and(NonEmptyList(head, tail)).some
    }

  private object verifyAndDerive {

    def flag(
        usedParams: Set[Name],
        long: Param.LongFlag,
        short: Defaultable.Optional[Param.ShortFlag],
    ): Either[Name, (Set[Name], Param.LongFlag, Option[Param.ShortFlag])] =
      for {
        usedParams <- verify.long(usedParams, long.name)
        (usedParams, short) <-
          short match {
            case Defaultable.None        => (usedParams, None).asRight
            case Defaultable.Some(short) => verify.short(usedParams, short.name).map((_, short.some))
            case Defaultable.Auto        => derive.flag(usedParams, long.name).asRight
          }
      } yield (usedParams, long, short)

    def value(
        usedParams: Set[Name],
        long: Param.LongWithValue,
        short: Defaultable.Optional[Param.ShortWithValue],
    ): Either[Name, (Set[Name], Param.LongWithValue, Option[Param.ShortWithValue])] =
      for {
        usedParams <- verify.long(usedParams, long.name)
        (usedParams, short) <-
          short match {
            case Defaultable.None        => (usedParams, None).asRight
            case Defaultable.Some(short) => verify.short(usedParams, short.name).map((_, short.some))
            case Defaultable.Auto        => derive.value(usedParams, long.name).asRight
          }
      } yield (usedParams, long, short)

    def toggle(
        usedParams: Set[Name],
        long: Param.LongToggle,
        short: Defaultable.Optional[Param.ShortToggle],
    ): Either[Name, (Set[Name], Param.LongToggle, Option[Param.ShortToggle])] =
      for {
        usedParams <- verify.long(usedParams, long.trueName)
        usedParams <- verify.long(usedParams, long.falseName)
        (usedParams, short) <-
          short match {
            case Defaultable.None => (usedParams, None).asRight
            case Defaultable.Some(short) =>
              for {
                usedParams <- verify.short(usedParams, short.trueName)
                usedParams <- verify.short(usedParams, short.falseName)
              } yield (usedParams, short.some)
            case Defaultable.Auto => derive.toggle(usedParams, long.baseName).asRight
          }
      } yield (usedParams, long, short)

  }

  private object verify {

    def long(usedParams: Set[Name], long: LongName): Either[Name, Set[Name]] =
      if (usedParams.contains(long)) long.asLeft
      else (usedParams + long).asRight

    def short(usedParams: Set[Name], short: ShortName): Either[Name, Set[Name]] =
      if (usedParams.contains(short)) short.asLeft
      else (usedParams + short).asRight

  }

  private object derive {

    private def matchFirstChar(baseName: LongName): Either[ShortName.Digit, (ShortName.LowerLetter, ShortName.UpperLetter)] =
      baseName.firstChar match {
        case letter: ShortName.LowerLetter => (letter, letter.toUpper).asRight
        case letter: ShortName.UpperLetter => (letter.toLower, letter).asRight
        case digit: ShortName.Digit        => digit.asLeft
      }

    def flag(
        usedParams: Set[Name],
        baseName: LongName,
    ): (Set[Name], Option[Param.ShortFlag]) =
      matchFirstChar(baseName) match {
        case Right((lowerLetter, upperLetter)) =>
          if (!usedParams.contains(lowerLetter)) (usedParams + lowerLetter, Param.ShortFlag(lowerLetter).some)
          else if (!usedParams.contains(upperLetter)) (usedParams + upperLetter, Param.ShortFlag(upperLetter).some)
          else (usedParams, None)
        case Left(digit) =>
          if (!usedParams.contains(digit)) (usedParams + digit, Param.ShortFlag(digit).some)
          else (usedParams, None)
      }

    def value(
        usedParams: Set[Name],
        baseName: LongName,
    ): (Set[Name], Option[Param.ShortWithValue]) =
      matchFirstChar(baseName) match {
        case Right((lowerLetter, upperLetter)) =>
          if (!usedParams.contains(lowerLetter)) (usedParams + lowerLetter, Param.ShortWithValue(lowerLetter).some)
          else if (!usedParams.contains(upperLetter)) (usedParams + upperLetter, Param.ShortWithValue(upperLetter).some)
          else (usedParams, None)
        case Left(digit) =>
          if (!usedParams.contains(digit)) (usedParams + digit, Param.ShortWithValue(digit).some)
          else (usedParams, None)
      }

    def toggle(
        usedParams: Set[Name],
        baseName: LongName,
    ): (Set[Name], Option[Param.ShortToggle]) =
      matchFirstChar(baseName) match {
        case Right((lowerLetter, upperLetter)) =>
          if (!usedParams.contains(lowerLetter) && !usedParams.contains(upperLetter))
            (usedParams + lowerLetter + upperLetter, Param.ShortToggle(upperLetter, lowerLetter).some)
          else
            (usedParams, None)
        case Left(_) => (usedParams, None)
      }

  }

}
