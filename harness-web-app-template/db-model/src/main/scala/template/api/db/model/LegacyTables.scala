package template.api.db.model

import harness.email.EmailAddress
import harness.sql.*
import template.model as D

object LegacyTables {

  object user {

    final case class V1[F[_]](
        id: F[User.Id],
        firstName: F[String],
        lastName: F[String],
        username: F[String],
        lowerUsername: F[String],
        encryptedPassword: F[String],
        email: F[EmailAddress],
    ) extends Table.WithId[F, User.Id] {
      def show: String = s"'$username' ($id)"
    }
    object V1 extends Table.Companion.WithId[D.user.UserId, user.V1] {

      override implicit lazy val tableSchema: TableSchema[user.V1] =
        TableSchema.derived[user.V1]("user") {
          new user.V1.Cols(
            id = User.Id.pkCol,
            firstName = Col.string("first_name"),
            lastName = Col.string("last_name"),
            username = Col.string("username"),
            lowerUsername = Col.string("lower_username"),
            encryptedPassword = Col.string("encrypted_password"),
            email = Col.encoded[EmailAddress]("email"),
          )
        }

    }

    final case class V2[F[_]](
        id: F[User.Id],
        firstName: F[String],
        lastName: F[String],
        username: F[String],
        lowerUsername: F[String],
        encryptedPassword: F[String],
        email: F[EmailAddress],
        verificationEmailCodes: F[Option[Set[D.user.EmailVerificationCode]]],
    ) extends Table.WithId[F, User.Id] {
      def show: String = s"'$username' ($id)"
    }
    object V2 extends Table.Companion.WithId[D.user.UserId, user.V2] {

      override implicit lazy val tableSchema: TableSchema[user.V2] =
        TableSchema.derived[user.V2]("user") {
          new user.V2.Cols(
            id = User.Id.pkCol,
            firstName = Col.string("first_name"),
            lastName = Col.string("last_name"),
            username = Col.string("username"),
            lowerUsername = Col.string("lower_username"),
            encryptedPassword = Col.string("encrypted_password"),
            email = Col.encoded[EmailAddress]("email"),
            verificationEmailCodes = Col.json[Set[D.user.EmailVerificationCode]]("verification_email_codes").optional,
          )
        }

    }

    export template.api.db.model.User as V3

  }

  object session {

    export template.api.db.model.Session as V1

  }
  
  object paymentMethod {
    
    export template.api.db.model.PaymentMethod as V1
    
  }

}
