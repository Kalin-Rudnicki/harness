package harness.archive.domain.impl.storage

import harness.archive.domain.model.*
import harness.archive.domain.model.Gens.*
import harness.archive.domain.storage.*
import harness.zio.{test as _, *}
import harness.zio.test.*
import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

object UserStorageContract extends Contract[UserStorage] {

  override val contract: TestSpec =
    suite("UserStorageContract")(
      suite("insert")(
        test("works") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              _ <- userStorage.insert(user)
            } yield assertCompletes
          }
        },
        test("fails if user is already inserted") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              _ <- userStorage.insert(user)
              res <- userStorage.insert(user).exit
            } yield assert(res)(fails(anything))
          }
        },
      ),
      suite("byUsername")(
        test("exists -> Some") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              _ <- userStorage.insert(user)
              res1 <- userStorage.byUsername(user.username.toLowerCase)
              res2 <- userStorage.byUsername(user.username.toUpperCase)
            } yield assert(res1)(isSome(equalTo(user))) &&
              assert(res2)(isSome(equalTo(user)))
          }
        },
        test("DNE -> None") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              _ <- userStorage.insert(user)
              res <- userStorage.byUsername("other")
            } yield assert(res)(isNone)
          }
        },
      ),
    )

}
