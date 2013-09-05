package models

import play.api.db.slick.Profile

case class RolePermission(id: Long, roleId: Long, permissionId: Long)

trait RolePermissionDao {

  self: Profile with RoleDao with PermissionDao =>

  import profile.simple._

  object RolePermissions extends Table[RolePermission]("role_permissions") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def roleId = column[Long]("role_id")

    def permissionId = column[Long]("permission_id")

    def * = id ~ roleId ~ permissionId <>(RolePermission.apply _, RolePermission.unapply _)

    def autoInc = id ~ roleId ~ permissionId returning id

    def roleFk = foreignKey("rp_role_fk", roleId, Roles)(_.id)

    def permissionFk = foreignKey("rp_perm_fk", permissionId, Permissions)(_.id)

  }

}