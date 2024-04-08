package harness.endpoint.spec

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import harness.endpoint.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.schema.{JsonSchema, RawSchema, Schema}
import harness.web.HttpMethod

final case class EndpointSpec[ET <: EndpointType.Any](
    method: HttpMethod,
    name: String,
    classification: EndpointSpec.Classification,
    description: Option[String],
    group: Option[String],
    inputWithCookiesCodec: NonBodyInputCodec[?, InputWithCookies[ET]],
    inputWithoutCookiesCodec: NonBodyInputCodec[?, InputWithoutCookies[ET]],
    inputBodyCodec: BodySchema[InputBody[ET]],
    outputBodyCodec: BodySchema[OutputBody[ET]],
    errorCodec: ErrorSchema[Error[ET]],
) { self =>

  def external: EndpointSpec[ET] =
    self.copy(classification = EndpointSpec.Classification.External)

  def internal: EndpointSpec[ET] =
    self.copy(classification = EndpointSpec.Classification.Internal)

  def hidden: EndpointSpec[ET] =
    self.copy(classification = EndpointSpec.Classification.Hidden)

  def describe(description: String): EndpointSpec[ET] =
    self.copy(description = description.some)

  def inGroup(group: String): EndpointSpec[ET] =
    self.copy(group = group.some)

}
object EndpointSpec {

  enum Classification extends Enum[Classification] { case External, Internal, Hidden }
  object Classification extends Enum.Companion[Classification]

  def apply(method: HttpMethod, name: String): Builder1[Unit] =
    new Builder1[Unit](
      method = method,
      name = name,
      encodeRPath = _ => Nil,
      decodePath = path => ((), path).some,
      rPathSchemas = Nil,
    )

  def get(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.GET, name)
  def put(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.PUT, name)
  def post(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.POST, name)
  def delete(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.DELETE, name)

  class Builder1[A] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      encodeRPath: A => List[String],
      decodePath: List[String] => Option[(A, List[String])],
      rPathSchemas: List[NonBodyInputCodec.PathSchema],
  ) extends Builder2[A, A](
        method,
        name,
        NonBodyInputCodec[A, A](
          encode = a => (encodeRPath(a).reverse, Map.empty, Map.empty, Map.empty),
          decodePath = decodePath(_) match {
            case Some((a, Nil)) => a.some
            case _              => None
          },
          decodeAll = (a, _, _, _) => NonBodyInputCodec.Result.Success(a),
          pathSchemas = rPathSchemas.reverse,
          querySchemas = Nil,
          headerSchemas = Nil,
        ),
      ) { self =>

    def /(p: String): Builder1[A] =
      new Builder1[A](
        method = method,
        name = name,
        encodeRPath = a => p :: self.encodeRPath(a),
        decodePath = self.decodePath(_).flatMap {
          case (a, `p` :: remaining) => (a, remaining).some
          case _                     => None
        },
        rPathSchemas = NonBodyInputCodec.PathSchema.Const(p) :: self.rPathSchemas,
      )

    def /[B](p: path[B])(implicit zip: Zip[A, B]): Builder1[zip.Out] =
      new Builder1[zip.Out](
        method = method,
        name = name,
        encodeRPath = { o =>
          val (a, b) = zip.unzip(o)
          p.schema.encode(b) :: self.encodeRPath(a)
        },
        decodePath = self.decodePath(_).flatMap {
          case (a, pHead :: pTail) => p.schema.decode(pHead).toOption.map(b => (zip.zip(a, b), pTail))
          case _                   => None
        },
        rPathSchemas = NonBodyInputCodec.PathSchema.Param(p.name, p.schema) :: self.rPathSchemas,
      )

    def /(p: pathRest)(implicit zip: Zip[A, List[String]]): Builder2[zip.Out, zip.Out] =
      new Builder1[zip.Out](
        method = method,
        name = name,
        encodeRPath = { o =>
          val (a, b) = zip.unzip(o)
          b.reverse ::: self.encodeRPath(a)
        },
        decodePath = self.decodePath(_).map { case (a, rest) =>
          (zip.zip(a, rest), Nil)
        },
        rPathSchemas = NonBodyInputCodec.PathSchema.Rest(p.name) :: self.rPathSchemas,
      )

    def /(p: pathRestNel)(implicit zip: Zip[A, NonEmptyList[String]]): Builder2[zip.Out, zip.Out] =
      new Builder1[zip.Out](
        method = method,
        name = name,
        encodeRPath = { o =>
          val (a, b) = zip.unzip(o)
          b.toList.reverse ::: self.encodeRPath(a)
        },
        decodePath = self.decodePath(_).flatMap {
          case (a, pHead :: pTail) => (zip.zip(a, NonEmptyList(pHead, pTail)), Nil).some
          case _                   => None
        },
        rPathSchemas = NonBodyInputCodec.PathSchema.RestNonEmpty(p.name) :: self.rPathSchemas,
      )

  }

  class Builder2[Path, All] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      codec: NonBodyInputCodec[Path, All],
  ) extends Builder3[Path, All, All](
        method,
        name,
        codec,
        codec,
      ) { self =>

    private def makeShared[B](
        mySchema: NonBodyInputCodec.OtherSchema.QuerySchema,
        encode: B => List[String],
        decode: List[String] => NonBodyInputCodec.Result[B],
    )(implicit zip: Zip[All, B]): Builder2[Path, zip.Out] =
      new Builder2[Path, zip.Out](
        method = method,
        name = name,
        codec = NonBodyInputCodec[Path, zip.Out](
          encode = { o =>
            val (a, b) = zip.unzip(o)
            val (path, queries, headers, cookies) = self.codec.encode(a)
            (
              path,
              encode(b) match {
                case Nil    => queries
                case values => queries.updated(mySchema.key, values)
              },
              headers,
              cookies,
            )
          },
          decodePath = self.codec.decodePath,
          decodeAll = (path, queries, headers, cookies) =>
            for {
              a <- self.codec.decodeAll(path, queries, headers, cookies)
              b <- decode(queries.getOrElse(mySchema.key, Nil))
            } yield zip.zip(a, b),
          pathSchemas = self.codec.pathSchemas,
          querySchemas = self.codec.querySchemas :+ mySchema,
          headerSchemas = self.codec.headerSchemas,
        ),
      )

    def /?[B](q: query[B])(implicit zip: Zip[All, B]): Builder2[Path, zip.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.QuerySchema(q.name, q.schema, NonBodyInputCodec.Requirement.Required)
      makeShared[B](
        mySchema,
        q.schema.encode(_) :: Nil,
        {
          case value :: Nil => NonBodyInputCodec.Result.fromEither(mySchema, q.schema.decode(value))
          case Nil          => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.Missing)
          case _ :: _ :: _  => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.DoesNotAcceptMultiple)
        },
      )
    }

    def /??[B](q: query[B])(implicit zip: Zip[All, Option[B]]): Builder2[Path, zip.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.QuerySchema(q.name, q.schema, NonBodyInputCodec.Requirement.Optional)
      makeShared[Option[B]](
        mySchema,
        _.map(q.schema.encode).toList,
        {
          case value :: Nil => NonBodyInputCodec.Result.fromEither(mySchema, q.schema.decode(value).map(_.some))
          case Nil          => NonBodyInputCodec.Result.Success(None)
          case _ :: _ :: _  => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.DoesNotAcceptMultiple)
        },
      )
    }

    def /?*[B](q: query[B])(implicit zip: Zip[All, List[B]]): Builder2[Path, zip.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.QuerySchema(q.name, q.schema, NonBodyInputCodec.Requirement.Many)
      makeShared[List[B]](
        mySchema,
        _.map(q.schema.encode),
        list => NonBodyInputCodec.Result.fromEither(mySchema, list.traverse(q.schema.decode)),
      )
    }

    def /?**[B](q: query[B])(implicit zip: Zip[All, NonEmptyList[B]]): Builder2[Path, zip.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.QuerySchema(q.name, q.schema, NonBodyInputCodec.Requirement.ManyNonEmpty)
      makeShared[NonEmptyList[B]](
        mySchema,
        _.map(q.schema.encode).toList,
        {
          case head :: tail => NonBodyInputCodec.Result.fromEither(mySchema, NonEmptyList(head, tail).traverse(q.schema.decode))
          case Nil          => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.Missing)
        },
      )
    }

  }

  class Builder3[Path, AllWithCookies, AllWithoutCookies] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      inputWithCookiesCodec: NonBodyInputCodec[Path, AllWithCookies],
      inputWithoutCookiesCodec: NonBodyInputCodec[Path, AllWithoutCookies],
  ) extends Builder4[Path, AllWithCookies, AllWithoutCookies, BodyType.None](
        method,
        name,
        inputWithCookiesCodec,
        inputWithoutCookiesCodec,
        BodySchema.None,
      ) { self =>

    private def makeCodec[All, B](
        mySchema: NonBodyInputCodec.OtherSchema.HeaderBasedSchema,
        codec: NonBodyInputCodec[Path, All],
        encode: B => (List[String], Option[String]),
        decode: (List[String], Option[String]) => NonBodyInputCodec.Result[B],
    )(implicit zip: Zip[All, B]): NonBodyInputCodec[Path, zip.Out] =
      NonBodyInputCodec[Path, zip.Out](
        encode = { o =>
          val (a, b) = zip.unzip(o)
          val (path, queries, headers, cookies) = codec.encode(a)
          val (h, c) = encode(b)
          (
            path,
            queries,
            h match {
              case Nil    => headers
              case values => headers.updated(mySchema.key, values)
            },
            c match {
              case Some(c) => cookies.updated(mySchema.key, c)
              case None    => cookies
            },
          )
        },
        decodePath = codec.decodePath,
        decodeAll = (path, queries, headers, cookies) =>
          for {
            a <- codec.decodeAll(path, queries, headers, cookies)
            b <- decode(headers.getOrElse(mySchema.key.toLowerCase, Nil), cookies.get(mySchema.key))
          } yield zip.zip(a, b),
        pathSchemas = codec.pathSchemas,
        querySchemas = codec.querySchemas,
        headerSchemas = codec.headerSchemas :+ mySchema,
      )

    def /#[B](h: header[B])(implicit zipC: Zip[AllWithCookies, B], zipH: Zip[AllWithoutCookies, B]): Builder3[Path, zipC.Out, zipH.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.HeaderSchema(h.name, h.schema, NonBodyInputCodec.Requirement.Required)
      val parseB: (List[String], Option[String]) => NonBodyInputCodec.Result[B] = {
        case (value :: Nil, _) => NonBodyInputCodec.Result.fromEither(mySchema, h.schema.decode(value))
        case (Nil, _)          => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.Missing)
        case (_ :: _ :: _, _)  => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.DoesNotAcceptMultiple)
      }
      new Builder3[Path, zipC.Out, zipH.Out](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, B](
          mySchema,
          inputWithCookiesCodec,
          b => (h.schema.encode(b) :: Nil, None),
          parseB,
        ),
        inputWithoutCookiesCodec = makeCodec[AllWithoutCookies, B](
          mySchema,
          inputWithoutCookiesCodec,
          b => (h.schema.encode(b) :: Nil, None),
          parseB,
        ),
      )
    }
    def /#[B](h: cookie[B])(implicit zipC: Zip[AllWithCookies, B]): Builder3[Path, zipC.Out, AllWithoutCookies] = {
      val mySchema = NonBodyInputCodec.OtherSchema.CookieSchema(h.name, h.schema, true)
      new Builder3[Path, zipC.Out, AllWithoutCookies](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, B](
          mySchema,
          inputWithCookiesCodec,
          b => (Nil, h.schema.encode(b).some),
          {
            case (_, Some(value)) => NonBodyInputCodec.Result.fromEither(mySchema, h.schema.decode(value))
            case (_, None)        => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.Missing)
          },
        ),
        inputWithoutCookiesCodec = self.inputWithoutCookiesCodec,
      )
    }
    def /#[B](h: headerOrCookie[B])(implicit zipC: Zip[AllWithCookies, B]): Builder3[Path, zipC.Out, AllWithoutCookies] = {
      val mySchema = NonBodyInputCodec.OtherSchema.HeaderOrCookieSchema(h.name, h.schema, true)
      new Builder3[Path, zipC.Out, AllWithoutCookies](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, B](
          mySchema,
          inputWithCookiesCodec,
          b => (Nil, h.schema.encode(b).some),
          {
            case (_, Some(value)) => NonBodyInputCodec.Result.fromEither(mySchema, h.schema.decode(value))
            case (_, None)        => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.Missing)
          },
        ),
        inputWithoutCookiesCodec = self.inputWithoutCookiesCodec,
      )
    }

    def /#?[B](h: header[B])(implicit zipC: Zip[AllWithCookies, Option[B]], zipH: Zip[AllWithoutCookies, Option[B]]): Builder3[Path, zipC.Out, zipH.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.HeaderSchema(h.name, h.schema, NonBodyInputCodec.Requirement.Optional)
      val parseB: (List[String], Option[String]) => NonBodyInputCodec.Result[Option[B]] = {
        case (value :: Nil, _) => NonBodyInputCodec.Result.fromEither(mySchema, h.schema.decode(value).map(_.some))
        case (Nil, _)          => NonBodyInputCodec.Result.Success(None)
        case (_ :: _ :: _, _)  => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.DoesNotAcceptMultiple)
      }
      new Builder3[Path, zipC.Out, zipH.Out](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, Option[B]](
          mySchema,
          inputWithCookiesCodec,
          b => (b.map(h.schema.encode).toList, None),
          parseB,
        ),
        inputWithoutCookiesCodec = makeCodec[AllWithoutCookies, Option[B]](
          mySchema,
          inputWithoutCookiesCodec,
          b => (b.map(h.schema.encode).toList, None),
          parseB,
        ),
      )
    }
    def /#?[B](h: cookie[B])(implicit zipC: Zip[AllWithCookies, Option[B]]): Builder3[Path, zipC.Out, AllWithoutCookies] = {
      val mySchema = NonBodyInputCodec.OtherSchema.CookieSchema(h.name, h.schema, false)
      new Builder3[Path, zipC.Out, AllWithoutCookies](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, Option[B]](
          mySchema,
          inputWithCookiesCodec,
          b => (Nil, b.map(h.schema.encode)),
          {
            case (_, Some(value)) => NonBodyInputCodec.Result.fromEither(mySchema, h.schema.decode(value).map(_.some))
            case (_, None)        => NonBodyInputCodec.Result.Success(None)
          },
        ),
        inputWithoutCookiesCodec = self.inputWithoutCookiesCodec,
      )
    }
    def /#?[B](h: headerOrCookie[B])(implicit zipC: Zip[AllWithCookies, Option[B]]): Builder3[Path, zipC.Out, AllWithoutCookies] = {
      val mySchema = NonBodyInputCodec.OtherSchema.HeaderOrCookieSchema(h.name, h.schema, false)
      new Builder3[Path, zipC.Out, AllWithoutCookies](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, Option[B]](
          mySchema,
          inputWithCookiesCodec,
          b => (Nil, b.map(h.schema.encode)),
          {
            case (_, Some(value)) => NonBodyInputCodec.Result.fromEither(mySchema, h.schema.decode(value).map(_.some))
            case (_, None)        => NonBodyInputCodec.Result.Success(None)
          },
        ),
        inputWithoutCookiesCodec = self.inputWithoutCookiesCodec,
      )
    }

    def /#*[B](h: header[B])(implicit zipC: Zip[AllWithCookies, List[B]], zipH: Zip[AllWithoutCookies, List[B]]): Builder3[Path, zipC.Out, zipH.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.HeaderSchema(h.name, h.schema, NonBodyInputCodec.Requirement.Many)
      val parseB: (List[String], Option[String]) => NonBodyInputCodec.Result[List[B]] = { (values, _) =>
        NonBodyInputCodec.Result.fromEither(mySchema, values.traverse(h.schema.decode))
      }
      new Builder3[Path, zipC.Out, zipH.Out](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, List[B]](
          mySchema,
          inputWithCookiesCodec,
          b => (b.map(h.schema.encode), None),
          parseB,
        ),
        inputWithoutCookiesCodec = makeCodec[AllWithoutCookies, List[B]](
          mySchema,
          inputWithoutCookiesCodec,
          b => (b.map(h.schema.encode), None),
          parseB,
        ),
      )
    }

    def /#**[B](h: header[B])(implicit zipC: Zip[AllWithCookies, NonEmptyList[B]], zipH: Zip[AllWithoutCookies, NonEmptyList[B]]): Builder3[Path, zipC.Out, zipH.Out] = {
      val mySchema = NonBodyInputCodec.OtherSchema.HeaderSchema(h.name, h.schema, NonBodyInputCodec.Requirement.ManyNonEmpty)
      val parseB: (List[String], Option[String]) => NonBodyInputCodec.Result[NonEmptyList[B]] = {
        case (vHead :: vTail, _) => NonBodyInputCodec.Result.fromEither(mySchema, NonEmptyList(vHead, vTail).traverse(h.schema.decode))
        case (Nil, _)            => NonBodyInputCodec.Result.Fail(mySchema, NonBodyInputCodec.FailureReason.Missing)
      }
      new Builder3[Path, zipC.Out, zipH.Out](
        method = method,
        name = name,
        inputWithCookiesCodec = makeCodec[AllWithCookies, NonEmptyList[B]](
          mySchema,
          inputWithCookiesCodec,
          b => (b.map(h.schema.encode).toList, None),
          parseB,
        ),
        inputWithoutCookiesCodec = makeCodec[AllWithoutCookies, NonEmptyList[B]](
          mySchema,
          inputWithoutCookiesCodec,
          b => (b.map(h.schema.encode).toList, None),
          parseB,
        ),
      )
    }

    // =====|  |=====

    def /<--[B](b: body[B]): Builder4[Path, AllWithCookies, AllWithoutCookies, BodyType.Encoded[B]] =
      new Builder4[Path, AllWithCookies, AllWithoutCookies, BodyType.Encoded[B]](
        method = method,
        name = name,
        inputWithCookiesCodec = inputWithCookiesCodec,
        inputWithoutCookiesCodec = inputWithoutCookiesCodec,
        inputBodyCodec = BodySchema.Encoded(b.schema),
      )
    def /<--(b: body.stream.type): Builder4[Path, AllWithCookies, AllWithoutCookies, BodyType.Stream] =
      new Builder4[Path, AllWithCookies, AllWithoutCookies, BodyType.Stream](
        method = method,
        name = name,
        inputWithCookiesCodec = inputWithCookiesCodec,
        inputWithoutCookiesCodec = inputWithoutCookiesCodec,
        inputBodyCodec = BodySchema.Stream,
      )
    def /<--(b: body.none.type): Builder4[Path, AllWithCookies, AllWithoutCookies, BodyType.None] =
      new Builder4[Path, AllWithCookies, AllWithoutCookies, BodyType.None](
        method = method,
        name = name,
        inputWithCookiesCodec = inputWithCookiesCodec,
        inputWithoutCookiesCodec = inputWithoutCookiesCodec,
        inputBodyCodec = BodySchema.None,
      )

  }

  class Builder4[Path, AllWithCookies, AllWithoutCookies, InputBody <: BodyType] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      inputWithCookiesCodec: NonBodyInputCodec[Path, AllWithCookies],
      inputWithoutCookiesCodec: NonBodyInputCodec[Path, AllWithoutCookies],
      inputBodyCodec: BodySchema[InputBody],
  ) extends Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, BodyType.None](
        method,
        name,
        inputWithCookiesCodec,
        inputWithoutCookiesCodec,
        inputBodyCodec,
        BodySchema.None,
      ) { self =>

    def /-->[B](b: body[B]): Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, BodyType.Encoded[B]] =
      new Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, BodyType.Encoded[B]](
        method = method,
        name = name,
        inputWithCookiesCodec = inputWithCookiesCodec,
        inputWithoutCookiesCodec = inputWithoutCookiesCodec,
        inputBodyCodec = inputBodyCodec,
        outputBodyCodec = BodySchema.Encoded(b.schema),
      )
    def /-->(b: body.stream.type): Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, BodyType.Stream] =
      new Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, BodyType.Stream](
        method = method,
        name = name,
        inputWithCookiesCodec = inputWithCookiesCodec,
        inputWithoutCookiesCodec = inputWithoutCookiesCodec,
        inputBodyCodec = inputBodyCodec,
        outputBodyCodec = BodySchema.Stream,
      )
    def /-->(b: body.none.type): Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, BodyType.None] =
      new Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, BodyType.None](
        method = method,
        name = name,
        inputWithCookiesCodec = inputWithCookiesCodec,
        inputWithoutCookiesCodec = inputWithoutCookiesCodec,
        inputBodyCodec = inputBodyCodec,
        outputBodyCodec = BodySchema.None,
      )

  }

  class Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody <: BodyType, OutputBody <: BodyType] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      inputWithCookiesCodec: NonBodyInputCodec[Path, AllWithCookies],
      inputWithoutCookiesCodec: NonBodyInputCodec[Path, AllWithoutCookies],
      inputBodyCodec: BodySchema[InputBody],
      outputBodyCodec: BodySchema[OutputBody],
  ) {

    def /!-->[B](b: errorBody[B]): EndpointSpec[EndpointType[AllWithCookies, AllWithoutCookies, InputBody, OutputBody, B]] =
      EndpointSpec[EndpointType[AllWithCookies, AllWithoutCookies, InputBody, OutputBody, B]](
        method = method,
        name = name,
        classification = Classification.External,
        description = None,
        group = None,
        inputWithCookiesCodec = inputWithCookiesCodec,
        inputWithoutCookiesCodec = inputWithoutCookiesCodec,
        inputBodyCodec = inputBodyCodec,
        outputBodyCodec = outputBodyCodec,
        errorCodec = b.schema,
      )

  }

}

final case class path[A](name: String, schema: Schema[A])
object path {
  def apply[A](name: String)(implicit schema: RawSchema[A]): path[A] = path(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): path[A] = path(name, schema)
}
final case class pathRest(name: String)
final case class pathRestNel(name: String)

final case class query[A](name: String, schema: Schema[A])
object query {
  def apply[A](name: String)(implicit schema: RawSchema[A]): query[A] = query(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): query[A] = query(name, schema)
}

final case class header[A](name: String, schema: Schema[A])
object header {
  def raw[A](name: String)(implicit schema: RawSchema[A]): header[A] = header(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): header[A] = header(name, schema)
}

final case class cookie[A](name: String, schema: Schema[A])
object cookie {
  def raw[A](name: String)(implicit schema: RawSchema[A]): cookie[A] = cookie(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): cookie[A] = cookie(name, schema)
}

final case class headerOrCookie[A](name: String, schema: Schema[A])
object headerOrCookie {
  def raw[A](name: String)(implicit schema: RawSchema[A]): headerOrCookie[A] = headerOrCookie(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): headerOrCookie[A] = headerOrCookie(name, schema)
}

final case class body[A](schema: Schema[A])
object body {
  def raw[A](implicit schema: RawSchema[A]): body[A] = body(schema)
  def json[A](implicit schema: JsonSchema[A]): body[A] = body(schema)
  case object stream
  case object none
}

final case class errorBody[A](schema: ErrorSchema[A])
object errorBody {
  def json[A](implicit schema: ErrorSchema.ForJson[A]): errorBody[A] = errorBody(schema)
  val none: errorBody[Nothing] = errorBody(ErrorSchema.ForNothing)
}
