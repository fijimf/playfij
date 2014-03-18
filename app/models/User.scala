package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile
import securesocial.core._
import securesocial.core.PasswordInfo
import securesocial.core.OAuth2Info
import securesocial.core.OAuth1Info

case class User(uid: Option[Long] = None,
                iid: IdentityId,
                firstName: String,
                lastName: String,
                fullName: String,
                email: Option[String],
                avatarUrl: Option[String],
                authMethod: AuthenticationMethod,
                oAuth1Info: Option[OAuth1Info],
                oAuth2Info: Option[OAuth2Info],
                passwordInfo: Option[PasswordInfo] = None) extends Identity {
  def identityId: IdentityId = iid
}

object User {
  def apply(i: Identity): User = User(None, i.identityId, i.firstName, i.lastName, i.fullName, i.email, i.avatarUrl, i.authMethod, i.oAuth1Info, i.oAuth2Info)
}

case class UserDao(model: Model) {

  import model._
  import model.profile.simple._

  def findById(id: Long)(implicit s: scala.slick.session.Session) = {
    val q = for {
      user <- Users
      if user.uid === id
    } yield user

    q.firstOption
  }

  def findByUserId(userId: IdentityId)(implicit s: scala.slick.session.Session): Option[User] = {
    val q = for {
      user <- Users
      if (user.userId === userId.userId) && (user.providerId === userId.providerId)
    } yield user

    q.firstOption
  }

  def all(implicit s: scala.slick.session.Session) = {
    val q = for {
      user <- Users
    } yield user

    q.list
  }

  def save(i: Identity)(implicit s: scala.slick.session.Session): User = this.save(User(i))

  def save(user: User)(implicit s: scala.slick.session.Session) = {
    findByUserId(user.identityId) match {
      case None => {
        val uid = Users.autoInc.insert(user)
        user.copy(uid = Some(uid))
      }
      case Some(existingUser) => {
        val userRow = for {
          u <- Users
          if u.uid === existingUser.uid
        } yield u

        val updatedUser = user.copy(uid = existingUser.uid)
        userRow.update(updatedUser)
        updatedUser
      }
    }
  }


}
