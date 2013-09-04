package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile

case class Permission(id: Long,
                 permission: String) {
  require(StringUtils.isNotBlank(permission))
}

trait RoleDao {

  self: Profile =>

  import profile.simple._

  object Permissions extends Table[Permissions]("permissions") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def permission = column[String]("permission")

    def * = id ~ permission <> (Permission.apply _, Permission.unapply _)

    def autoInc = id ~ permission <>(Permission.apply _, Permission.unapply _)

    def permissionIndex = index("prm_role", permission, unique = true)
  }
}
