package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile
import scala.slick.lifted


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

case class Alias(id: Long, teamId: Long, alias: String) {
  require(StringUtils.isNotBlank(alias))
}


case class TeamDao(model: Model) {

  import model._
  import model.profile.simple._

  val team = for {t <- Teams} yield t
  val teamAndAlias = for {(t, a) <- Teams leftJoin Aliases on (_.id === _.teamId)} yield (t, a.alias.?)

  def pairsToMap(lst: List[(Team, Option[String])]): Map[Team, List[String]] = lst.groupBy(_._1).mapValues(_.map(_._2).flatten)

  def list(implicit s: scala.slick.session.Session): List[Team] = team.sortBy(_.name).list

  def listWithAliases(implicit s: scala.slick.session.Session): Map[Team, List[String]] = pairsToMap(teamAndAlias.list)

  def findWithAliases(key: String)(implicit s: scala.slick.session.Session): Option[(Team, List[String])] =
    pairsToMap(teamAndAlias.where(_._1.key === key).list).headOption

  def find(key: String)(implicit s: scala.slick.session.Session): Option[Team] = team.where(_.key === key).firstOption

  def update(team: Team)(implicit s: scala.slick.session.Session) = Teams.where(_.id === team.id).update(team)

  def insert(team: Team)(implicit s: scala.slick.session.Session) {
    Teams.autoInc.insert(team.key, team.name, team.longName, team.nickname, team.primaryColor, team.secondaryColor, team.logoUrl, team.officialUrl, team.officialTwitter)
  }

  def updateAliases(team: Team, aliases: List[String])(implicit s: scala.slick.session.Session) {
    Aliases.where(_.teamId === team.id).delete
    aliases.foreach(a => Aliases.autoInc.insert(team.id, a))
  }

  def addAlias(alias: String, teamKey: String)(implicit s: scala.slick.session.Session) {
    val team = Query(Teams).where(_.key === teamKey).firstOption
    team.foreach(t => Aliases.autoInc.insert((t.id, alias)))
  }

  def insertAliases(team: Team, aliases: List[String])(implicit s: scala.slick.session.Session) {
    Teams.autoInc.insert(team.key, team.name, team.longName, team.nickname, team.primaryColor, team.secondaryColor, team.logoUrl, team.officialUrl, team.officialTwitter)
    aliases.foreach(a => Aliases.autoInc.insert(team.id, a))
  }

  def delete(id: String)(implicit s: scala.slick.session.Session) {
    Aliases.where(_.teamId === id.toLong).delete
    ConferenceAssociations.where(_.teamId === id.toLong).delete
    Teams.where(_.id === id.toLong).delete
  }

}
