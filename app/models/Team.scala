package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile


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
  }
  def list(implicit s:scala.slick.session.Session): List[Team] = {
    Query(Teams).sortBy(_.name).to[List]
  }

  def find(key: String)(implicit s:scala.slick.session.Session): Option[Team] = {
    Query(Teams).where(_.key === key).firstOption
  }

  def update(team: Team)(implicit s:scala.slick.session.Session) {
    Teams.where(_.id === team.id).update(team)
  }

  def insert(team: Team)(implicit s:scala.slick.session.Session) {
    Teams.autoInc.insert(team.key, team.name, team.longName, team.nickname, team.primaryColor, team.secondaryColor, team.logoUrl, team.officialUrl, team.officialTwitter)
  }

  def delete(id: String)(implicit s:scala.slick.session.Session) {
    Teams.where(_.id === id.toLong).delete
  }

}

object TeamDao {
  def apply(p:ExtendedProfile): TeamDao = {
    new TeamDao with Profile {
      val profile = p
    }
  }
}