package models

import scala.slick.lifted.DDL
import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile


case class Team(id: Long, key: String, name: String, longName: String, nickname: String, primaryColor: Option[String], secondaryColor: Option[String], logoUrl: Option[String], officialUrl: Option[String], officialTwitter: Option[String]) {
  require(StringUtils.isNotBlank(key), "Key cannot be blank")
  require(StringUtils.isNotBlank(name), "Name cannot be blank")
  require(StringUtils.isNotBlank(longName), "Long name cannot be blank")
  require(StringUtils.isNotBlank(nickname), "Nickname cannot be blank")
  require(primaryColor.map(StringUtils.isNotBlank).getOrElse(true), "Primary color cannot be blank")
  require(secondaryColor.map(StringUtils.isNotBlank).getOrElse(true), "Secondary color cannot be blank")
  require(officialUrl.map(StringUtils.isNotBlank).getOrElse(true), "Official URL cannot be blank")
  require(officialTwitter.map(StringUtils.isNotBlank).getOrElse(true), "Official Twitter cannot be blank")
  require(logoUrl.map(StringUtils.isNotBlank).getOrElse(true), "Logo URL cannot be blank")
}

trait TeamDao {

  this: Profile =>

  import profile.simple._


  object Teams extends Table[Team]("teams") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def key = column[String]("team_key")

    def name = column[String]("name")

    def longName = column[String]("long_name")

    def nickname = column[String]("nickname")

    def primaryColor = column[Option[String]]("primary_color")

    def secondaryColor = column[Option[String]]("secondary_color")

    def logoUrl = column[Option[String]]("logo_url")

    def officialUrl = column[Option[String]]("official_url")

    def officialTwitter = column[Option[String]]("official_twitter")

    def * = id ~ key ~ name ~ longName ~ nickname ~ primaryColor ~ secondaryColor ~ logoUrl ~ officialUrl ~ officialTwitter <>(Team.apply _, Team.unapply _)

    def autoInc = key ~ name ~ longName ~ nickname ~ primaryColor ~ secondaryColor ~ logoUrl ~ officialUrl ~ officialTwitter returning id

    def keyIndex = index("tea_key", key, unique = true)
    def nameIndex = index("tea_name", name, unique = true)
    def longNameIndex = index("tea_long_name", longName, unique = true)

    override def ddl: DDL = {
      var constraints: DDL = DDL(
        Nil,
//        List(
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_key\" CHECK (\"team_key\"<>'')",
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_name\" CHECK (\"name\"<>'')",
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_long_ame\" CHECK (\"long_name\"<>'')",
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_url\" CHECK (\"official_url\"<>'')",
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_twitter\" CHECK (\"official_twitter\"<>'')",
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_p_color\" CHECK (\"primary_color\"<>'')",
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_s_color\" CHECK (\"secondary_color\"<>'')",
//          "ALTER TABLE \"teams\" ADD CONSTRAINT \"team_check_logo\" CHECK (\"logo_url\"<>'')"
//        ),
//        List(
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_key\"",
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_name\"",
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_long_ame\"",
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_url\"",
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_twitter\"",
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_p_color\"",
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_s_color\"",
//          "ALTER TABLE \"teams\" DROP CONSTRAINT \"team_check_logo\""
//        ),
        Nil)
      super.ddl ++ constraints
    }

  }

}