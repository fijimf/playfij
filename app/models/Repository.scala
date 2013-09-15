package models

import scala.slick.driver.ExtendedProfile
import scala.slick.session.Database._
import play.api.db.slick.Profile
import scala.slick.jdbc.meta.MTable
import scraping.{ScrapingUtil, NcaaTeamScraper}
import play.api.Logger

class Repository(p: ExtendedProfile)
  extends TeamDao
  with ResultDao
  with AliasDao
  with RoleDao
  with UserDao
  with PermissionDao
  with Profile {
  val logger: Logger = Logger("Repository")

  val profile = p

  val conferenceDao = ConferenceDao(p)

  import profile.simple._

  def teamKeys = Query(Teams).sortBy(_.name).map(_.key).to[List]

  def checkDatabase(): DatabaseStatus = {
    val tables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet

    DatabaseStatus(
      None,
      if (tables.contains("teams")) Query(Teams.length).firstOption else None,
      None,
      if (tables.contains("games")) Query(Games.length).firstOption else None,
      if (tables.contains("results")) Query(Results.length).firstOption else None,
      if (tables.contains("aliases")) Query(Aliases.length).firstOption else None,
      None)
  }

  def rebuildDatabase() {
    val modelTables: List[Table[_]] = List( Teams, Games, Results, Aliases, Roles, Users, Permissions)
    val dbTables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet
    val toDrop = modelTables.filter(t => dbTables.contains(t.tableName)).map(_.ddl)
    toDrop match {
      case Nil =>
      case d :: Nil => d.drop
      case d :: ds => ds.foldLeft(d)(_ ++ _).drop
    }
    modelTables.map(_.ddl).reduceLeft(_ ++ _).create
  }


  def scrapeNcaaTeamsAndConferences() {
    val conferences: List[Conference] = NcaaTeamScraper.conferenceMap.values.toSet.map((s: String) => Conference(0, ScrapingUtil.nameToKey(s), s, s, None, None, None)).toList
    val teams: List[Team] = NcaaTeamScraper.teamList
    logger.info("Loaded team data")
    upsertConferenceListByKey(conferences)
    upsertTeamListByKey(teams)
  }

  def upsertConferenceListByKey(conferences: List[Conference]) {
    conferences.foreach(c => {
      conferenceDao.find(c.key) match {
        case Some(d) => conferenceDao.update(c)
        case None => conferenceDao.insert(c)
      }
    })
  }



  def getTeams: List[Team] = {
    Query(Teams).sortBy(_.name).to[List]
  }

  def getTeam(key: String): Option[Team] = {
    Query(Teams).where(_.key === key).firstOption
  }

  def updateTeam(team: Team) {
    Teams.where(_.id === team.id).update(team)
  }

  def insertTeam(team: Team) {
    Teams.autoInc.insert(team.key, team.name, team.longName, team.nickname, team.primaryColor, team.secondaryColor, team.logoUrl, team.officialUrl, team.officialTwitter)
  }

  def deleteTeam(id: String) {
    Teams.where(_.id === id.toLong).delete
  }

  def upsertTeamListByKey(teamData: List[Team]) {
    teamData.foreach((t: Team) => {
      if (t.id > 0) {
        Query(Teams).filter(_.key === t.key).firstOption match {
          case Some(u) => Teams.where(_.key === t.key).update(t)
          case None => insertTeam(t)
        }
      } else {
        insertTeam(t)
      }
    })
  }
}

case class DatabaseStatus(seasonCount: Option[Int],
                          teamCount: Option[Int],
                          conferenceCount: Option[Int],
                          gameCount: Option[Int],
                          resultCount: Option[Int],
                          aliasCount: Option[Int],
                          quoteCount: Option[Int])
