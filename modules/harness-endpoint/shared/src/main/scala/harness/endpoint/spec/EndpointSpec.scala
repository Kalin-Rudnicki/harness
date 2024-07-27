package harness.endpoint.spec

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.{Enum, Zip}
import harness.endpoint.error.ApiInternalDefect
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.schema.*
import harness.web.*
import zio.Tag

final case class EndpointSpec[ET <: EndpointType.Any](
    method: HttpMethod,
    name: String,
    classification: EndpointSpec.Classification,
    description: Option[String],
    group: Option[String],
    pathCodec: PathCodec[Path[ET]],
    queryCodec: QueryCodec[Query[ET]],
    authHeaderCodec: HeaderCodec[Auth[ET]],
    headerCodec: HeaderCodec[Header[ET]],
    inputBodyCodec: BodyCodec[InputBody[ET]],
    outputBodyCodec: BodyCodec[OutputBody[ET]],
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
      pathCodec = PathCodec.Empty,
    )

  def get(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.GET, name)
  def put(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.PUT, name)
  def post(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.POST, name)
  def delete(name: String): Builder1[Unit] = EndpointSpec(HttpMethod.DELETE, name)

  class Builder1[PathT] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      pathCodec: PathCodec[PathT],
  ) extends Builder2[PathT, Unit](
        method,
        name,
        pathCodec,
        QueryCodec.Empty,
      ) { self => // TODO (KR) : extends Builder2

    private def withPathCodec[P2](pathCodec: PathCodec[P2]): Builder1[P2] =
      new Builder1[P2](
        method = method,
        name = name,
        pathCodec = pathCodec,
      )

    final def /(p: String): Builder1[PathT] = withPathCodec(self.pathCodec / PathCodec.Const(p))

    // TODO (KR) : path/pathRest/pathRestNonEmpty
    final def /[P2](p: PathCodec[P2])(implicit z: Zip[PathT, P2]): Builder1[z.Out] =
      withPathCodec { self.pathCodec / p }

    final def /[P2](p: path[P2])(implicit z: Zip[PathT, P2]): Builder1[z.Out] =
      self / PathCodec.Param(p.name, p.schema)
    final def /(p: pathRest)(implicit z: Zip[PathT, List[String]]): Builder1[z.Out] =
      self / PathCodec.Rest(p.name)
    final def /(p: pathRestNonEmpty)(implicit z: Zip[PathT, NonEmptyList[String]]): Builder1[z.Out] =
      self / PathCodec.RestNonEmpty(p.name)

  }

  class Builder2[PathT, QueryT] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      pathCodec: PathCodec[PathT],
      queryCodec: QueryCodec[QueryT],
  ) extends Builder3[PathT, QueryT, Unit](
        method,
        name,
        pathCodec,
        queryCodec,
        HeaderCodec.Empty,
      ) { self =>

    private def withQueryCodec[Q2](queryCodec: QueryCodec[Q2]): Builder2[PathT, Q2] =
      new Builder2[PathT, Q2](
        method = method,
        name = name,
        pathCodec = pathCodec,
        queryCodec = queryCodec,
      )

    final def /?[Q2](queryCodec: QueryCodec[Q2])(implicit z: Zip[QueryT, Q2]): Builder2[PathT, z.Out] =
      withQueryCodec { self.queryCodec ++ queryCodec }

    final def /?[Q2](q: query[Q2])(implicit z: Zip[QueryT, Q2]): Builder2[PathT, z.Out] =
      self /? QueryCodec.Required(q.name, q.schema)
    final def /??[Q2](q: query[Q2])(implicit z: Zip[QueryT, Option[Q2]]): Builder2[PathT, z.Out] =
      self /? QueryCodec.Optional(q.name, q.schema)
    final def /?*[Q2](q: query[Q2])(implicit z: Zip[QueryT, List[Q2]]): Builder2[PathT, z.Out] =
      self /? QueryCodec.Many(q.name, q.schema)
    final def /?**[Q2](q: query[Q2])(implicit z: Zip[QueryT, NonEmptyList[Q2]]): Builder2[PathT, z.Out] =
      self /? QueryCodec.ManyNonEmpty(q.name, q.schema)

  }

  class Builder3[PathT, QueryT, AuthT] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      pathCodec: PathCodec[PathT],
      queryCodec: QueryCodec[QueryT],
      authHeaderCodec: HeaderCodec[AuthT],
  ) extends Builder4[PathT, QueryT, AuthT, Unit](
        method,
        name,
        pathCodec,
        queryCodec,
        authHeaderCodec,
        HeaderCodec.Empty,
      ) { self =>

    private def withAuthHeaderCodec[A2](authHeaderCodec: HeaderCodec[A2]): Builder3[PathT, QueryT, A2] =
      new Builder3[PathT, QueryT, A2](
        method = method,
        name = name,
        pathCodec = pathCodec,
        queryCodec = queryCodec,
        authHeaderCodec = authHeaderCodec,
      )

    final def /!#[A2](authHeaderCodec: HeaderCodec[A2])(implicit z: Zip[AuthT, A2]): Builder3[PathT, QueryT, z.Out] =
      withAuthHeaderCodec { self.authHeaderCodec ++ authHeaderCodec }

    final def /!#[A2](h: header[A2])(implicit z: Zip[AuthT, A2]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.HeaderRequired(h.name, h.schema)
    final def /!#?[A2](h: header[A2])(implicit z: Zip[AuthT, Option[A2]]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.HeaderOptional(h.name, h.schema)
    final def /!#*[A2](h: header[A2])(implicit z: Zip[AuthT, List[A2]]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.HeaderMany(h.name, h.schema)
    final def /!#**[A2](h: header[A2])(implicit z: Zip[AuthT, NonEmptyList[A2]]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.HeaderManyNonEmpty(h.name, h.schema)

    final def /!#[A2](h: cookie[A2])(implicit z: Zip[AuthT, A2]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.CookieRequired(h.name, h.schema)
    final def /!#?[A2](h: cookie[A2])(implicit z: Zip[AuthT, Option[A2]]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.CookieOptional(h.name, h.schema)

    final def /!#[A2](h: headerOrCookie[A2])(implicit z: Zip[AuthT, A2]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.HeaderOrCookieRequired(h.name, h.schema)
    final def /!#?[A2](h: headerOrCookie[A2])(implicit z: Zip[AuthT, Option[A2]]): Builder3[PathT, QueryT, z.Out] =
      self /!# HeaderCodec.HeaderOrCookieOptional(h.name, h.schema)

  }

  class Builder4[PathT, QueryT, AuthT, HeaderT] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      pathCodec: PathCodec[PathT],
      queryCodec: QueryCodec[QueryT],
      authHeaderCodec: HeaderCodec[AuthT],
      headerCodec: HeaderCodec[HeaderT],
  ) extends Builder5[PathT, QueryT, AuthT, HeaderT, BodyType.None](
        method,
        name,
        pathCodec,
        queryCodec,
        authHeaderCodec,
        headerCodec,
        BodyCodec.None,
      ) { self =>

    private def withAuthHeaderCodec[H2](headerCodec: HeaderCodec[H2]): Builder4[PathT, QueryT, AuthT, H2] =
      new Builder4[PathT, QueryT, AuthT, H2](
        method = method,
        name = name,
        pathCodec = pathCodec,
        queryCodec = queryCodec,
        authHeaderCodec = authHeaderCodec,
        headerCodec = headerCodec,
      )

    final def /#[H2](headerCodec: HeaderCodec[H2])(implicit z: Zip[HeaderT, H2]): Builder4[PathT, QueryT, AuthT, z.Out] =
      withAuthHeaderCodec { self.headerCodec ++ headerCodec }

    final def /#[H2](h: header[H2])(implicit z: Zip[HeaderT, H2]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.HeaderRequired(h.name, h.schema)
    final def /#?[H2](h: header[H2])(implicit z: Zip[HeaderT, Option[H2]]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.HeaderOptional(h.name, h.schema)
    final def /#*[H2](h: header[H2])(implicit z: Zip[HeaderT, List[H2]]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.HeaderMany(h.name, h.schema)
    final def /#**[H2](h: header[H2])(implicit z: Zip[HeaderT, NonEmptyList[H2]]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.HeaderManyNonEmpty(h.name, h.schema)

    final def /#[H2](h: cookie[H2])(implicit z: Zip[HeaderT, H2]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.CookieRequired(h.name, h.schema)
    final def /#?[H2](h: cookie[H2])(implicit z: Zip[HeaderT, Option[H2]]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.CookieOptional(h.name, h.schema)

    final def /#[H2](h: headerOrCookie[H2])(implicit z: Zip[HeaderT, H2]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.HeaderOrCookieRequired(h.name, h.schema)
    final def /#?[H2](h: headerOrCookie[H2])(implicit z: Zip[HeaderT, Option[H2]]): Builder4[PathT, QueryT, AuthT, z.Out] =
      self /# HeaderCodec.HeaderOrCookieOptional(h.name, h.schema)

  }

  class Builder5[PathT, QueryT, AuthT, HeaderT, InputBodyT <: BodyType] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      pathCodec: PathCodec[PathT],
      queryCodec: QueryCodec[QueryT],
      authHeaderCodec: HeaderCodec[AuthT],
      headerCodec: HeaderCodec[HeaderT],
      inputBodyCodec: BodyCodec[InputBodyT],
  ) extends Builder6[PathT, QueryT, AuthT, HeaderT, InputBodyT, BodyType.None](
        method,
        name,
        pathCodec,
        queryCodec,
        authHeaderCodec,
        headerCodec,
        inputBodyCodec,
        BodyCodec.None,
      ) { self =>

    private def withInputBodyCodec[I2 <: BodyType](inputBodyCodec: BodyCodec[I2]): Builder5[PathT, QueryT, AuthT, HeaderT, I2] =
      new Builder5[PathT, QueryT, AuthT, HeaderT, I2](
        method = method,
        name = name,
        pathCodec = pathCodec,
        queryCodec = queryCodec,
        authHeaderCodec = authHeaderCodec,
        headerCodec = headerCodec,
        inputBodyCodec = inputBodyCodec,
      )

    final def /<--[I2](b: body[I2]): Builder6[PathT, QueryT, AuthT, HeaderT, BodyType.Encoded[I2], BodyType.None] =
      withInputBodyCodec { BodyCodec.Encoded(b.schema) }

    final def /<--(b: body.stream.type): Builder6[PathT, QueryT, AuthT, HeaderT, BodyType.Stream, BodyType.None] =
      withInputBodyCodec { BodyCodec.Stream }
    final def /<--(b: body.none.type): Builder6[PathT, QueryT, AuthT, HeaderT, BodyType.None, BodyType.None] =
      withInputBodyCodec { BodyCodec.None }

  }

  class Builder6[PathT, QueryT, AuthT, HeaderT, InputBodyT <: BodyType, OutputBodyT <: BodyType] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      pathCodec: PathCodec[PathT],
      queryCodec: QueryCodec[QueryT],
      authHeaderCodec: HeaderCodec[AuthT],
      headerCodec: HeaderCodec[HeaderT],
      inputBodyCodec: BodyCodec[InputBodyT],
      outputBodyCodec: BodyCodec[OutputBodyT],
  ) extends Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT, OutputBodyT, ApiInternalDefect](
        method,
        name,
        pathCodec,
        queryCodec,
        authHeaderCodec,
        headerCodec,
        inputBodyCodec,
        outputBodyCodec,
        ApiInternalDefect.errorSchema,
      ) { self =>

    private def withOutputBodyCodec[O2 <: BodyType](outputBodyCodec: BodyCodec[O2]): Builder6[PathT, QueryT, AuthT, HeaderT, InputBodyT, O2] =
      new Builder6[PathT, QueryT, AuthT, HeaderT, InputBodyT, O2](
        method = method,
        name = name,
        pathCodec = pathCodec,
        queryCodec = queryCodec,
        authHeaderCodec = authHeaderCodec,
        headerCodec = headerCodec,
        inputBodyCodec = inputBodyCodec,
        outputBodyCodec = outputBodyCodec,
      )

    final def /-->[O2](b: body[O2]): Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT, BodyType.Encoded[O2], ApiInternalDefect] =
      withOutputBodyCodec { BodyCodec.Encoded(b.schema) }
    final def /-->(b: body.stream.type): Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT, BodyType.Stream, ApiInternalDefect] =
      withOutputBodyCodec { BodyCodec.Stream }
    final def /-->(b: body.none.type): Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT, BodyType.None, ApiInternalDefect] =
      withOutputBodyCodec { BodyCodec.None }

  }

  class Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT <: BodyType, OutputBodyT <: BodyType, ErrorT] private[EndpointSpec] (
      method: HttpMethod,
      name: String,
      pathCodec: PathCodec[PathT],
      queryCodec: QueryCodec[QueryT],
      authHeaderCodec: HeaderCodec[AuthT],
      headerCodec: HeaderCodec[HeaderT],
      inputBodyCodec: BodyCodec[InputBodyT],
      outputBodyCodec: BodyCodec[OutputBodyT],
      errorCodec: ErrorSchema[ErrorT],
  ) {

    private[endpoint] def toEndpointSpec: EndpointSpec[EndpointType[PathT, QueryT, AuthT, HeaderT, InputBodyT, OutputBodyT, ErrorT]] =
      EndpointSpec(
        method = method,
        name = name,
        classification = Classification.External,
        description = None,
        group = None,
        pathCodec = pathCodec,
        queryCodec = queryCodec,
        authHeaderCodec = authHeaderCodec,
        headerCodec = headerCodec,
        inputBodyCodec = inputBodyCodec,
        outputBodyCodec = outputBodyCodec,
        errorCodec = errorCodec,
      )

    private def withErrorCodec[E2](errorCodec: ErrorSchema[E2]): Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT, OutputBodyT, E2] =
      new Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT, OutputBodyT, E2](
        method = method,
        name = name,
        pathCodec = pathCodec,
        queryCodec = queryCodec,
        authHeaderCodec = authHeaderCodec,
        headerCodec = headerCodec,
        inputBodyCodec = inputBodyCodec,
        outputBodyCodec = outputBodyCodec,
        errorCodec = errorCodec,
      )

    final def /!-->[E](b: errorBody[E]): EndpointSpec[EndpointType[PathT, QueryT, AuthT, HeaderT, InputBodyT, OutputBodyT, E]] =
      withErrorCodec { b.schema }.toEndpointSpec

  }

}

// =====| Builders |=====

final case class path[A](name: String, schema: Schema[A])
object path {
  def apply[A](name: String)(implicit schema: RawSchema[A]): path[A] = path(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): path[A] = path(name, schema)
}
final case class pathRest(name: String)
final case class pathRestNonEmpty(name: String)

final case class query[A](name: String, schema: Schema[A])
object query {
  def apply[A](name: String)(implicit schema: RawSchema[A]): query[A] = query(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): query[A] = query(name, schema)
}

final case class header[A](name: String, schema: Schema[A])
object header {

  def raw[A](name: String)(implicit schema: RawSchema[A]): header[A] = header(name, schema)
  def json[A](name: String)(implicit schema: JsonSchema[A]): header[A] = header(name, schema)

  // TODO (KR) : have a special schema for this
  def bearer[A: Tag](name: String)(implicit schema: JsonSchema[A]): header[JWT[A]] = header.raw[JWT[A]](name)

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
  def json[A](implicit schema: ErrorSchema[A]): errorBody[A] = errorBody(schema)
}
