package harness.endpoint

import harness.core.*
import harness.endpoint.spec.{given, *}
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import java.util.UUID

object TmpMain extends scala.App {

  final case class Tmp1[F[_ <: EndpointType.Any]](
      route1: F[EndpointType[Unit, Unit, BodyType.None, BodyType.None, Nothing]],
      route2: F[EndpointType[Int, Int, BodyType.None, BodyType.None, Nothing]],
      route3: F[EndpointType[(Int, String), (Int, String), BodyType.None, BodyType.None, Nothing]],
      route4: F[EndpointType[(Int, String, Option[String]), (Int, String), BodyType.None, BodyType.None, Nothing]],
      route5: F[EndpointType[(Int, String, Option[String]), (Int, String), BodyType.Encoded[UUID], BodyType.None, Nothing]],
      route6: F[EndpointType[(Int, String, Option[String]), (Int, String), BodyType.Encoded[UUID], BodyType.Encoded[UUID], Nothing]],
      route7: F[EndpointType[(Int, String, Option[String]), (Int, String), BodyType.Encoded[UUID], BodyType.Encoded[UUID], Nothing]],
  )

  final case class Tmp2[F[_ <: EndpointType.Any]](
      tmp1: Tmp1[F],
  )

  val tmp1Spec: Tmp1[EndpointSpec] =
    Tmp1[EndpointSpec](
      route1 = EndpointSpec.get("Route 1") / "route-1",
      route2 = EndpointSpec.get("Route 2") / "route-2" / path[Int]("my-id"),
      route3 = EndpointSpec.get("Route 3") / "route-3" / path[Int]("my-id") /? query[String]("q-p"),
      route4 = EndpointSpec.get("Route 3") / "route-3" / path[Int]("my-id") /? query[String]("q-p") /#? cookie.raw[String]("auth"),
      route5 = EndpointSpec.get("Route 3") / "route-3" / path[Int]("my-id") /? query[String]("q-p") /#? cookie.raw[String]("auth") /<-- body.json[UUID],
      route6 = EndpointSpec.get("Route 3") / "route-3" / path[Int]("my-id") /? query[String]("q-p") /#? cookie.raw[String]("auth") /<-- body.json[UUID] /--> body.raw[UUID],
      route7 = (EndpointSpec.get("Route 3") / "route-3" / path[Int]("my-id") /? query[String]("q-p") /#? cookie.raw[String]("auth") /<-- body.json[UUID] /--> body.raw[UUID])
        .describe("Test")
        .internal,
    )

  val tmp2Spec: Tmp2[EndpointSpec] =
    Tmp2[EndpointSpec](
      tmp1 = "api" /: "tmp-1" /: tmp1Spec,
    )

  val tmp2SpecZip: Tmp2[K11ET.Zip[EndpointSpec, EndpointSpec]] =
    Zipper.zip(tmp2Spec, tmp2Spec)

  val flat: List[EndpointSpec[EndpointType.Any]] = Flatten.flatten(tmp2Spec)

  def showPath(p: NonBodyInputCodec.PathSchema): String =
    p match {
      case NonBodyInputCodec.PathSchema.Const(const)        => const
      case NonBodyInputCodec.PathSchema.Param(name, schema) => s"{{ $name : ${schema.tag} }}"
      case NonBodyInputCodec.PathSchema.Rest(name)          => s"{{ $name : * }}"
      case NonBodyInputCodec.PathSchema.RestNonEmpty(name)  => s"{{ $name : ** }}"
    }

  def show(es: EndpointSpec[EndpointType.Any]): Unit =
    println(
      IndentedString.inline(
        IndentedString.Break,
        s"${es.method} (${es.name}) <${es.classification}>",
        IndentedString.indented(
          s"path: /${es.inputWithCookiesCodec.pathSchemas.map(showPath).mkString("/")}",
        ),
      ),
    )

  flat.foreach(show)

}
