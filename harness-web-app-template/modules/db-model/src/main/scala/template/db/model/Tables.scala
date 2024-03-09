package template.db.model

import harness.email.EmailAddress
import harness.payments.model.ids as StripeIds
import harness.payments.model as PM
import harness.sql.*
import template.api.model as Api
import template.domain.model as Domain

final case class User[F[_]](
    id: F[User.Id],
    firstName: F[String],
    lastName: F[String],
    username: F[String],
    lowerUsername: F[String],
    encryptedPassword: F[String],
    email: F[EmailAddress],
    verificationEmailCodes: F[Option[Set[Api.user.EmailVerificationCode]]],
    stripeCustomerId: F[Option[StripeIds.CustomerId]],
) extends Table.WithId[F, User.Id]
object User extends Table.Companion.WithId[Api.user.UserId, User] {

  def fromDomain(user: Domain.User): User.Identity =
    new User.Identity(
      id = user.id,
      firstName = user.firstName,
      lastName = user.lastName,
      username = user.username,
      lowerUsername = user.lowerUsername,
      encryptedPassword = user.encryptedPassword,
      email = user.email,
      verificationEmailCodes = user.verificationEmailCodes,
      stripeCustomerId = user.stripeCustomerId,
    )

  def toDomain(user: User.Identity): Domain.User =
    Domain.User(
      id = user.id,
      firstName = user.firstName,
      lastName = user.lastName,
      username = user.username,
      lowerUsername = user.lowerUsername,
      encryptedPassword = user.encryptedPassword,
      email = user.email,
      verificationEmailCodes = user.verificationEmailCodes,
      stripeCustomerId = user.stripeCustomerId,
    )

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
        verificationEmailCodes = Col.json[Set[Api.user.EmailVerificationCode]]("verification_email_codes").optional,
        stripeCustomerId = Col.string("stripe_customer_id").imapAuto[StripeIds.CustomerId].optional,
      )
    }

}

final case class Session[F[_]](
    id: F[Session.Id],
    userId: F[User.Id],
    token: F[Api.user.UserToken],
) extends Table.WithId[F, Session.Id]
object Session extends Table.Companion.WithId[Api.user.SessionId, Session] {

  def fromDomain(session: Domain.Session): Session.Identity =
    new Session.Identity(
      id = session.id,
      userId = session.userId,
      token = session.token,
    )

  def toDomain(session: Session.Identity): Domain.Session =
    Domain.Session(
      id = session.id,
      userId = session.userId,
      token = session.token,
    )

  override implicit lazy val tableSchema: TableSchema[Session] =
    TableSchema.derived[Session]("session") {
      new Session.Cols(
        id = Session.Id.pkCol,
        userId = User.Id.fkCol("user_id"),
        token = Col.string("token").imapAuto[Api.user.UserToken],
      )
    }

}

final case class PaymentMethod[F[_]](
    id: F[PaymentMethod.Id],
    userId: F[User.Id],
    stripeId: F[StripeIds.PaymentMethodId],
    typeString: F[String],
    typeDetails: F[Option[PM.result.TypeDetails]],
) extends Table.WithId[F, PaymentMethod.Id]
object PaymentMethod extends Table.Companion.WithId[Api.paymentMethod.PaymentMethodId, PaymentMethod] {

  def fromDomain(paymentMethod: Domain.PaymentMethod): PaymentMethod.Identity =
    new PaymentMethod.Identity(
      id = paymentMethod.id,
      userId = paymentMethod.userId,
      stripeId = paymentMethod.stripeId,
      typeString = paymentMethod.typeString,
      typeDetails = paymentMethod.typeDetails,
    )

  def toDomain(paymentMethod: PaymentMethod.Identity): Domain.PaymentMethod =
    Domain.PaymentMethod(
      id = paymentMethod.id,
      userId = paymentMethod.userId,
      stripeId = paymentMethod.stripeId,
      typeString = paymentMethod.typeString,
      typeDetails = paymentMethod.typeDetails,
    )

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
