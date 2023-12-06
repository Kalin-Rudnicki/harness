package template.api.db.model

import harness.email.EmailAddress
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
