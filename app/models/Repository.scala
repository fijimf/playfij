package models

import scala.slick.driver.ExtendedProfile
import scala.slick.lifted.DDL
import scala.slick.session.Database._
import play.api.db.slick.Profile
import scala.slick.jdbc.meta.MTable
import scraping.{ScrapingUtil, NcaaTeamScraper}
import play.api.Logger

class Repository(p: ExtendedProfile)
  extends SeasonDao
  with ConferenceDao
  with TeamDao
  with GameDao
  with ResultDao
  with AliasDao
  with QuoteDao
  with RoleDao
  with UserDao
  with PermissionDao
  with Profile {
  val logger: Logger = Logger("Repository")

  val profile = p

  import profile.simple._

  def createSeason(year: String): Long = {
    Seasons.autoInc.insert(year)
  }

  def createConference(conference: Conference) {
    Conferences.autoInc.insert(conference.key, conference.name, conference.shortName, conference.officialUrl, conference.officialTwitter, conference.logoUrl)
  }

  def checkDatabase(): DatabaseStatus = {
    val tables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet

    DatabaseStatus(
      if (tables.contains("seasons")) Query(Seasons.length).firstOption else None,
      if (tables.contains("teams")) Query(Teams.length).firstOption else None,
      if (tables.contains("conferences")) Query(Conferences.length).firstOption else None,
      if (tables.contains("games")) Query(Games.length).firstOption else None,
      if (tables.contains("results")) Query(Results.length).firstOption else None,
      if (tables.contains("aliases")) Query(Aliases.length).firstOption else None,
      if (tables.contains("quotes")) Query(Quotes.length).firstOption else None
    )
  }

  def rebuildDatabase() {
    val tables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet
    val drops: List[DDL] = List(Seasons, Conferences, Teams, Games, Results, Aliases, Quotes, Roles, Users, Permissions).filter(t => tables.contains(t.tableName)).map(_.ddl)
    drops match {
      case Nil =>
      case d :: Nil => d.drop
      case d :: ds => ds.foldLeft(d)(_ ++ _).drop
    }
    List(Seasons, Conferences, Teams, Games, Results, Roles, Users, Permissions).map(_.ddl).reduceLeft(_ ++ _).create
  }


  def scrapeNcaaTeamsAndConferences() {
    val conferences: List[Conference] = NcaaTeamScraper.conferenceMap.values.toSet.map((s:String) => Conference(0, ScrapingUtil.nameToKey(s), s, s, None, None, None)).toList
    val teams: List[Team] = NcaaTeamScraper.teamList
    logger.info("Loaded team data")
    upsertConferenceListByKey(conferences)
    upsertTeamListByKey(teams)
  }

  def upsertConferenceListByKey(conferences: List[Conference]) {
    conferences.foreach(c => {
      Query(Conferences).filter(_.key === c.key).firstOption match {
        case Some(d) => Conferences.where(_.key === c.key).update(c)
        case None => Conferences.autoInc.insert(c.key, c.name, c.shortName, c.officialUrl, c.officialTwitter, c.logoUrl)
      }
    })
  }

  def getTeams: List[Team] = {
    Query(Teams).to[List]
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
