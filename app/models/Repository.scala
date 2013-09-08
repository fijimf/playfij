package models

import scala.slick.driver.ExtendedProfile
import scala.slick.lifted.DDL
import scala.slick.session.Database._
import play.api.db.slick.Profile
import scala.slick.jdbc.meta.MTable
import scraping.{ScrapingUtil, NcaaTeamScraper}
import play.api.Logger
import scraping.NcaaTeamScraper.TeamData

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
    val conferenceMap: Map[String, String] = NcaaTeamScraper.conferenceMap
    val teamData: List[Map[String, String]] = NcaaTeamScraper.teamData
    logger.info("Loaded team data")
    upsertConferences(conferenceMap)
    upsertTeams(teamData)
  }

  def upsertConferences(conferenceMap: Map[String, String]) {

    val values: Iterable[String] = conferenceMap.values
    val toSet: Set[String] = values.toSet
    toSet.foreach(name => {
      val key = ScrapingUtil.nameToKey(name)
      val oc = (for (c <- Conferences if c.key === key) yield c).firstOption
      if (oc.isDefined) {
        Logger("Repository").info("Updating conference '%s'".format(key))
        val conference: Conference = oc.get.copy(name = name, shortName = name)
        Conferences.where(_.key === key).update(conference)
      } else {
        Logger("Repository").info("Inserting conference '%s'".format(key))
        Conferences.autoInc.insert((key, name, name, None, None, None))
      }
    })
  }


  def getTeams: List[Team] = {
    Query(Teams).to[List]
  }

  def getTeam(key: String): Option[Team] = {
    Query(Teams).where(_.key === key).firstOption
  }

  def createTeam(team: Team) {
    Teams.autoInc.insert(team.key, team.name, team.longName, team.nickname, team.primaryColor, team.secondaryColor, team.officialUrl, team.officialTwitter, team.logoUrl)
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

  def upsertTeams(teamData: List[Map[String, String]]) {
    teamData.foreach((data: Map[String, String]) => {
      val key = data.get(TeamData.Key)
      val logoUrl = data.get(TeamData.LogoUrl)
      val longName = data.get(TeamData.LongName)
      val name = data.get(TeamData.Name)
      val nickname = data.get(TeamData.Nickname)
      val officialUrl = data.get(TeamData.OfficialUrl)
      val primaryColor = data.get(TeamData.PrimaryColor)
      val secondaryColor = data.get(TeamData.SecondaryColor)
      if (key.isDefined) {
        val ot = (for (t <- Teams if t.key === key.get) yield t).firstOption
        if (ot.isDefined) {
          Logger("Repository").info("Updating team '%s'".format(key.get))
          val team = copyIfDefined[String]((tm, x) => tm.copy(name = x.get), name).
            andThen(copyIfDefined[String]((tm, x) => tm.copy(longName = x.get), longName)).
            andThen(copyIfDefined[String]((tm, x) => tm.copy(nickname = x.get), nickname)).
            andThen(copyIfDefined[String]((tm, x) => tm.copy(logoUrl = x), logoUrl)).
            andThen(copyIfDefined[String]((tm, x) => tm.copy(officialUrl = x), officialUrl)).
            andThen(copyIfDefined[String]((tm, x) => tm.copy(primaryColor = x), primaryColor)).
            andThen(copyIfDefined[String]((tm, x) => tm.copy(secondaryColor = x), secondaryColor)).apply(ot.get)
          Teams.where(_.key === key).update(team)
        } else {
          Logger("Repository").info("Inserting team '%s'".format(key))
          if (name.isDefined && longName.isDefined && nickname.isDefined) {
            Teams.autoInc.insert(key.get, name.get, longName.get, nickname.get, primaryColor, secondaryColor, logoUrl, officialUrl, None)
          }
        }
      }
    })
  }

  def copyIfDefined[T](f: (Team, Option[T]) => Team, t: Option[T]) =
    (team: Team) => if (t.isDefined) f(team, t) else team
}

case class DatabaseStatus(seasonCount: Option[Int],
                          teamCount: Option[Int],
                          conferenceCount: Option[Int],
                          gameCount: Option[Int],
                          resultCount: Option[Int],
                          aliasCount: Option[Int],
                          quoteCount: Option[Int])
