package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile

case class User(id: Long,
                name: String,
                password: String,
                email: String) {
  require(StringUtils.isNotBlank(name))
  require(StringUtils.isNotBlank(password))
  require(StringUtils.isNotBlank(email))
}

case class UserDao(model: Model) {

  import model._
  import model.profile.simple._

}
