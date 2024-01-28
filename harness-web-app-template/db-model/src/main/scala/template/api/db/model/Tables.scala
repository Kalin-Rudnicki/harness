package template.api.db.model

import harness.email.EmailAddress
import harness.payments.model.ids as StripeIds
import harness.payments.model as PM
import harness.sql.*
import java.util.UUID
import template.model as D

final case class User[F[_]](
    id: F[User.Id],
    firstName: F[String],
    lastName: F[String],
    username: F[String],
    lowerUsername: F[String],
    encryptedPassword: F[String],
    email: F[EmailAddress],
    verificationEmailCodes: F[Option[Set[D.user.EmailVerificationCode]]],
    stripeCustomerId: F[Option[StripeIds.CustomerId]],
) extends Table.WithId[F, User.Id] {
  def show: String = s"'$username' ($id)"
}
object User extends Table.Companion.WithId[D.user.UserId, User] {

  override implicit lazy val tableSchema: TableSchema[User] =
    TableSchema.derived[User]("user") {
      new User.Cols(
        id = User.Id.pkCol,
        firstName = Col.string("first_name"),
        lastName = Col.string("last_name"),
        username = Col.string("username"),
        lowerUsername = Col.string("lower_username"),
        encryptedPassword = Col.string("encrypted_password"),
        email = Col.encoded[EmailAddress]("email"),
        verificationEmailCodes = Col.json[Set[D.user.EmailVerificationCode]]("verification_email_codes").optional,
        stripeCustomerId = Col.string("stripe_customer_id").imapAuto[StripeIds.CustomerId].optional,
      )
    }

}

final case class Session[F[_]](
    id: F[Session.Id],
    userId: F[User.Id],
    token: F[String],
) extends Table.WithId[F, Session.Id]
object Session extends Table.Companion.WithId[D.user.SessionId, Session] {

  override implicit lazy val tableSchema: TableSchema[Session] =
    TableSchema.derived[Session]("session") {
      new Session.Cols(
        id = Session.Id.pkCol,
        userId = User.Id.fkCol("user_id"),
        token = Col.string("token"),
      )
    }

  def newForUser(user: User.Identity): Session.Identity =
    new Session.Identity(
      id = Session.Id.gen,
      userId = user.id,
      token = s"${UUID.randomUUID}:${UUID.randomUUID}",
    )

}

final case class PaymentMethod[F[_]](
    id: F[PaymentMethod.Id],
    userId: F[User.Id],
    stripeId: F[StripeIds.PaymentMethodId],
    typeString: F[String],
    typeDetails: F[Option[PM.result.TypeDetails]],
) extends Table.WithId[F, PaymentMethod.Id]
object PaymentMethod extends Table.Companion.WithId[D.paymentMethod.PaymentMethodId, PaymentMethod] {

  override implicit lazy val tableSchema: TableSchema[PaymentMethod] =
    TableSchema.derived[PaymentMethod]("payment_method") {
      new PaymentMethod.Cols(
        id = PaymentMethod.Id.pkCol,
        userId = User.Id.fkCol,
        stripeId = Col.string("stripe_id").imapAuto[StripeIds.PaymentMethodId],
        typeString = Col.string("type_string"),
        typeDetails = Col.jsonb[PM.result.TypeDetails]("type_details").optional,
      )
    }

}
