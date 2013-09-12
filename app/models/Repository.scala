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

  def teamKeys = Query(Teams).sortBy(_.name).map(_.key).to[List]
  def conferenceKeys = Query(Conferences).sortBy(_.name).map(_.key).to[List]
  def seasonKeys = Query(Seasons).sortBy(_.season).map(_.key).to[List]
  def quoteKeys = Query(Quotes).sortBy(_.id).map(_.id).to[List]

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


  def getQuotes: List[Quote] = {
    Query(Quotes).sortBy(_.id).to[List]
  }

  def getQuote(id: Long): Option[Quote] = {
    Query(Quotes).where(_.id === id).firstOption
  }

  def updateQuote(quote: Quote) {
    Quotes.where(_.id === quote.id).update(quote)
  }

  def insertQuote(quote: Quote) {
    Quotes.autoInc.insert(quote.quote, quote.source, quote.url)
  }

  def deleteQuote(id: String) {
    Quotes.where(_.id === id.toLong).delete
  }


  def getSeasons: List[Season] = {
    Query(Seasons).sortBy(_.key).to[List]
  }

  def getSeason(key: String): Option[Season] = {
    Query(Seasons).where(_.key === key).firstOption
  }

  def updateSeason(season: Season) {
    Seasons.where(_.id === season.id).update(season)
  }

  def insertSeason(season: Season) {
    Seasons.autoInc.insert(season.key, season.season, season.from, season.to)
  }

  def deleteSeason(id: String) {
    Seasons.where(_.id === id.toLong).delete
  }


  def getConferences: List[Conference] = {
    Query(Conferences).sortBy(_.name).to[List]
  }

  def getConference(key: String): Option[Conference] = {
    Query(Conferences).where(_.key === key).firstOption
  }

  def updateConference(conference: Conference) {
    Conferences.where(_.id === conference.id).update(conference)
  }

  def insertConference(conference: Conference) {
    Conferences.autoInc.insert(conference.key, conference.name, conference.shortName, conference.logoUrl, conference.officialUrl, conference.officialTwitter)
  }

  def deleteConference(id: String) {
    Conferences.where(_.id === id.toLong).delete
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
