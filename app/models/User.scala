package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile

case class User(id: Long,
                 name: String,
                 password: String,
                 email:String) {
  require(StringUtils.isNotBlank(name))
  require(StringUtils.isNotBlank(password))
  require(StringUtils.isNotBlank(emmail))
}

trait UserDao {

  self: Profile =>

  import profile.simple._

  object Users extends Table[Users]("users") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def name = column[String]("name")

    def password = column[String]("password")
    
    def email = column[String]("email")

    def * = id ~ name ~ password ~ email <>(User.apply _, User.unapply _)

    def autoInc = id ~ name ~ password ~ email  <>(User.apply _, User.unapply _)

    def nameIndex = index("usr_name", name, unique = true)
    
    def emailIndex = index("usr_email", email, unique = true)
  }
}
