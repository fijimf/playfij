package models

import play.api.db.slick.Profile

case class UserRole(id: Long, userId: Long, roleId: Long)

trait UserRoleDao {

  self: Profile with UserDao with RoleDao =>

  import profile.simple._

  object UserRoles extends Table[UserRole]("user_role") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def userId = column[Long]("user_id")

    def roleId = column[Long]("role_id")

    def * = id ~ userId ~ roleId  <>(UserRole.apply _, UserRole.unapply _)

    def autoInc = id ~ userId ~ roleId  returning id

    def roleFk = foreignKey("ur_role_fk", roleId, Roles)(_.id)

    def userFk = foreignKey("ur_user_fk", userId, Users)(_.id)

  }
}