package template.api.db.model

import harness.sql.*
import java.util.UUID

final case class User[F[_]](
    id: F[User.Id],
    firstName: F[String],
    lastName: F[String],
    username: F[String],
    lowerUsername: F[String],
    encryptedPassword: F[String],
    email: F[String],
) extends Table.WithId[F, User.Id] {
  def show: String = s"'$username' ($id)"
}
object User extends Table.Companion.WithId[User] {

  override implicit lazy val tableSchema: TableSchema[User] =
    TableSchema.derived[User]("user") {
      new User.Cols(
        id = User.Id.basicCol("id").primaryKey,
        firstName = Col.string("first_name"),
        lastName = Col.string("last_name"),
        username = Col.string("username"),
        lowerUsername = Col.string("lower_username"),
        encryptedPassword = Col.string("encrypted_password"),
        email = Col.string("email"),
      )
    }

}

final case class Session[F[_]](
    id: F[Session.Id],
    userId: F[User.Id],
    token: F[String],
) extends Table.WithId[F, Session.Id]
object Session extends Table.Companion.WithId[Session] {

  override implicit lazy val tableSchema: TableSchema[Session] =
    TableSchema.derived[Session]("session") {
      new Session.Cols(
        id = Session.Id.basicCol("id").primaryKey,
        userId = User.Id.basicCol("user_id").references(ForeignKeyRef("user", "id")),
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
