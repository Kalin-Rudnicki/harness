package template.domain.impl.storage

import harness.zio.{test as _, *}
import harness.zio.test.*
import template.api.model as Api
import template.domain.model.*
import template.domain.model.Gens.*
import template.domain.storage.*
import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

object PaymentMethodStorageContract extends Contract[UserStorage & PaymentMethodStorage] {

  val contract: TestSpec =
    suite("PaymentMethodStorageContract")(
      suite("insert")(
        test("works") {
          check(genUserAndPaymentMethod) { case (user, paymentMethod) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              paymentMethodStorage <- ZIO.service[PaymentMethodStorage]
              _ <- userStorage.insert(user)
              _ <- paymentMethodStorage.insert(paymentMethod)
            } yield assertCompletes
          }
        },
        test("fails if no user exists") {
          check(genUserAndPaymentMethod) { case (_, paymentMethod) =>
            for {
              paymentMethodStorage <- ZIO.service[PaymentMethodStorage]
              res <- paymentMethodStorage.insert(paymentMethod).exit
            } yield assert(res)(fails(anything))
          }
        },
      ),
      suite("getById")(
        test("works") {
          check(genUserAndPaymentMethod) { case (user, paymentMethod) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              paymentMethodStorage <- ZIO.service[PaymentMethodStorage]
              _ <- userStorage.insert(user)
              _ <- paymentMethodStorage.insert(paymentMethod)
              res <- paymentMethodStorage.getById(paymentMethod.id)
            } yield assert(res)(equalTo(paymentMethod))
          }
        },
        test("fails if no session exists") {
          check(genUserAndPaymentMethod) { case (user, paymentMethod) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              paymentMethodStorage <- ZIO.service[PaymentMethodStorage]
              _ <- userStorage.insert(user)
              _ <- paymentMethodStorage.insert(paymentMethod)
              otherPaymentMethodId <- Api.paymentMethod.PaymentMethodId.genZio
              res <- paymentMethodStorage.getById(otherPaymentMethodId).exit
            } yield assert(res)(fails(anything))
          }
        },
      ),
      suite("getForUser")(
        test("works - 0") {
          check(genUserAndPaymentMethod) { case (user, paymentMethod) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              paymentMethodStorage <- ZIO.service[PaymentMethodStorage]
              _ <- userStorage.insert(user)
              _ <- paymentMethodStorage.insert(paymentMethod)
              otherUserId <- Api.user.UserId.genZio
              res <- paymentMethodStorage.getForUser(otherUserId)
            } yield assert(res)(equalTo(Chunk.empty))
          }
        },
        test("works - 1") {
          check(genUserAndPaymentMethod) { case (user, paymentMethod) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              paymentMethodStorage <- ZIO.service[PaymentMethodStorage]
              _ <- userStorage.insert(user)
              _ <- paymentMethodStorage.insert(paymentMethod)
              res <- paymentMethodStorage.getForUser(user.id)
            } yield assert(res)(equalTo(Chunk(paymentMethod)))
          }
        },
        test("works - many") {
          check(
            for {
              user <- genUser
              paymentMethods <- Gen.chunkOfBounded(5, 10)(genPaymentMethod(user))
            } yield (user, paymentMethods),
          ) { case (user, paymentMethods) =>
            for {
              userStorage <- ZIO.service[UserStorage]
              paymentMethodStorage <- ZIO.service[PaymentMethodStorage]
              _ <- userStorage.insert(user)
              _ <- ZIO.foreachDiscard(paymentMethods)(paymentMethodStorage.insert)
              res <- paymentMethodStorage.getForUser(user.id)
            } yield assert(res)(hasSameElements(paymentMethods))
          }
        },
      ),
    )

}
