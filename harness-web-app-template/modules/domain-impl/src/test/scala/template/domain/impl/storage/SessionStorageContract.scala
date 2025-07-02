package template.domain.impl.storage

import harness.zio.test.*
import template.api.model as Api
import template.domain.model.*
import template.domain.model.Gens.*
import template.domain.storage.*
import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

object SessionStorageContract extends Contract[UserStorage & SessionStorage] {

  override def testSpec: TestSpec =
    suite("SessionStorageContract")(
      suite("insert")(
        test("works") {
          check(genUserAndSession) { case (user, session) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              sessionStorage <- ZIO.service[SessionStorage]
              _ <- userStorage.insert(user)
              _ <- sessionStorage.insert(session)
            } yield assertCompletes
          }
        },
        test("fails if no user exists") {
          check(genUserAndSession) { case (_, session) =>
            for {
              sessionStorage <- ZIO.service[SessionStorage]
              res <- sessionStorage.insert(session).exit
            } yield assert(res)(fails(anything))
          }
        },
      ),
      suite("sessionFromSessionToken")(
        test("exists -> Some") {
          check(genUserAndSession) { case (user, session) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              sessionStorage <- ZIO.service[SessionStorage]
              _ <- userStorage.insert(user)
              _ <- sessionStorage.insert(session)
              res <- sessionStorage.sessionFromSessionToken(session.token)
            } yield assert(res)(isSome(equalTo(session)))
          }
        },
        test("DNE -> None") {
          check(genUserAndSession) { case (user, session) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              sessionStorage <- ZIO.service[SessionStorage]
              _ <- userStorage.insert(user)
              _ <- sessionStorage.insert(session)
              res <- sessionStorage.sessionFromSessionToken(Api.user.UserToken("other"))
            } yield assert(res)(isNone)
          }
        },
      ),
      suite("userFromSessionToken")(
        test("exists -> Some") {
          check(genUserAndSession) { case (user, session) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              sessionStorage <- ZIO.service[SessionStorage]
              _ <- userStorage.insert(user)
              _ <- sessionStorage.insert(session)
              res <- sessionStorage.userFromSessionToken(session.token)
            } yield assert(res)(isSome(equalTo(user)))
          }
        },
        test("DNE -> None") {
          check(genUserAndSession) { case (user, session) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              sessionStorage <- ZIO.service[SessionStorage]
              _ <- userStorage.insert(user)
              _ <- sessionStorage.insert(session)
              res <- sessionStorage.userFromSessionToken(Api.user.UserToken("other"))
            } yield assert(res)(isNone)
          }
        },
      ),
      suite("deleteById")(
        test("works") {
          check(genUserAndSession) { case (user, session) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              sessionStorage <- ZIO.service[SessionStorage]
              _ <- userStorage.insert(user)
              _ <- sessionStorage.insert(session)
              _ <- sessionStorage.deleteById(session.id)
              res <- sessionStorage.userFromSessionToken(session.token)
            } yield assert(res)(isNone)
          }
        },
        test("fails if session doesn't exist") {
          check(genUserAndSession) { case (user, session) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              sessionStorage <- ZIO.service[SessionStorage]
              _ <- userStorage.insert(user)
              _ <- sessionStorage.insert(session)
              otherSessionId <- Api.user.SessionId.genZio
              res <- sessionStorage.deleteById(otherSessionId).exit
            } yield assert(res)(fails(anything))
          }
        },
      ),
    )

}
