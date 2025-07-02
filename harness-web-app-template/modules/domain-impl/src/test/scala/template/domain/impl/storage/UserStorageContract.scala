package template.domain.impl.storage

import cats.syntax.option.*
import harness.payments.model.ids.CustomerId
import harness.zio.test.*
import template.api.model as Api
import template.domain.model.*
import template.domain.model.Gens.*
import template.domain.storage.*
import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

object UserStorageContract extends Contract[UserStorage] {

  override def testSpec: TestSpec =
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
      suite("setEmailCodes")(
        test("works") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              _ <- userStorage.insert(user)
              code <- Api.user.EmailVerificationCode.genZio
              _ <- userStorage.setEmailCodes(user.id, Set(code).some)
              res1 <- userStorage.byUsername(user.username)
              _ <- userStorage.setEmailCodes(user.id, None)
              res2 <- userStorage.byUsername(user.username)
            } yield assert(res1.map(_.verificationEmailCodes))(isSome(isSome(equalTo(Set(code))))) &&
              assert(res2.map(_.verificationEmailCodes))(isSome(isNone))
          }

        },
        test("fails if no user exists") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              otherCode <- Api.user.EmailVerificationCode.genZio
              res <- userStorage.setEmailCodes(user.id, Set(otherCode).some).exit
            } yield assert(res)(fails(anything))
          }
        },
      ),
      suite("setStripeCustomerId")(
        test("works") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              _ <- userStorage.insert(user)
              customerId <- CustomerId.genZio
              _ <- userStorage.setStripeCustomerId(user.id, customerId.some)
              res1 <- userStorage.byUsername(user.username)
              _ <- userStorage.setStripeCustomerId(user.id, None)
              res2 <- userStorage.byUsername(user.username)
            } yield assert(res1.map(_.stripeCustomerId))(isSome(isSome(equalTo(customerId)))) &&
              assert(res2.map(_.stripeCustomerId))(isSome(isNone))
          }
        },
        test("fails if no user exists") {
          check(genUser) { user =>
            for {
              userStorage <- ZIO.service[UserStorage]
              otherCustomerId <- CustomerId.genZio
              res <- userStorage.setStripeCustomerId(user.id, otherCustomerId.some).exit
            } yield assert(res)(fails(anything))
          }
        },
      ),
    )

}
