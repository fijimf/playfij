package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile

case class Role(id: Long,
                 role: String) {
  require(StringUtils.isNotBlank(role))
}

trait RoleDao {

  self: Profile =>

  import profile.simple._

  object Roles extends Table[Roles]("roles") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def role = column[String]("role")

    def * = id ~ role  <> (Role.apply _, Role.unapply _)

    def autoInc = id ~ role <>(Role.apply _, Role.unapply _)

    def roleIndex = index("rol_role", name, unique = true)
  }
}
