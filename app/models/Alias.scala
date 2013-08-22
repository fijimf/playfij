package models

import org.apache.commons.lang3.StringUtils

case class Alias(id: Long,
                 teamId: Long,
                 alias: String) {
  require(StringUtils.isNotBlank(alias))
}

trait AliasDao {

  self: Profile with TeamDao =>

  import profile.simple._

  object Aliases extends Table[Alias]("alias") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def teamId = column[Long]("team_id")

    def alias = column[String]("alias")

    def * = id ~ teamId ~ alias <>(Alias.apply _, Alias.unapply _)

    def autoInc = id ~ teamId ~ alias <>(Alias.apply _, Alias.unapply _)

    def teamFk = foreignKey("als_team_fk", teamId, Teams)(_.id)

    def aliasIndex = index("als_alias", alias, unique = true)
  }

}
