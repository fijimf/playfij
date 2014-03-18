package plugins

import play.api.Application
import securesocial.core.{IdentityId, Identity, UserServicePlugin}
import securesocial.core.providers.Token
import models.User
import play.api.Logger
import models.Model
import models.UserDao

class UserService(application: Application) extends UserServicePlugin(application) {


  def find(id: IdentityId) = {
    import play.api.Play.current

    val model = new Model() {
      val profile = play.api.db.slick.DB.driver
    }
    val userDao: UserDao = UserDao(model)
    play.api.db.slick.DB.withSession {
      implicit s =>
        userDao.findByUserId(id)
    }
  }

  def save(user: Identity) = {
    import play.api.Play.current

    val model = new Model() {
      val profile = play.api.db.slick.DB.driver
    }
    val userDao: UserDao = UserDao(model)
    play.api.db.slick.DB.withSession {
      implicit s =>
        userDao.save(user)
    }
  }

  // Since we're not using username/password login, we don't need the methods below
  def findByEmailAndProvider(email: String, providerId: String) = None

  def save(token: Token) {}

  def findToken(token: String) = None

  def deleteToken(uuid: String) {}

  def deleteExpiredTokens() {}
}
