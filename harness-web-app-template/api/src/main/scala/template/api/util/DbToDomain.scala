package template.api.util

import template.api.db.model as M
import template.model as D

object DbToDomain {

  def user(user: M.User.Identity): D.user.User =
    D.user.User(
      id = user.id,
      firstName = user.firstName,
      lastName = user.lastName,
      username = user.username,
      email = user.email,
      emailIsVerified = user.verificationEmailCodes.isEmpty,
    )

}
