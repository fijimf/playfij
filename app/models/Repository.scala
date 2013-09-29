package models

import scala.slick.driver.ExtendedProfile
import scala.slick.session.Database._
import play.api.db.slick.Profile
import scala.slick.jdbc.meta.MTable
import scraping.{ScrapingUtil, NcaaTeamScraper}
import play.api.Logger

class Repository(p: ExtendedProfile)
  extends Profile {
  val logger: Logger = Logger("Repository")
  val profile = p
  import profile.simple._
  val m = new Profile
    with SeasonDao
    with ConferenceDao
    with TeamDao
    with ConferencesAssociationDao
    with GameDao
    with ResultDao
    with UserDao
    with QuoteDao {
    val profile: ExtendedProfile = p

    override def find(key:String)(implicit session:Session) = None
    override def list(implicit session:Session) = List.empty
    override def delete(key:String)(implicit session:Session) {}
  }

  val conferenceDao = ConferenceDao(p)
  val teamDao = TeamDao(p)

  def checkDatabase(): List[(String, Option[Int])] = {

    val mTables = List(m.Seasons, m.Conferences, m.Teams, m.ConferenceAssociations, m.Aliases, m.Games, m.Results, m.Users, m.Quotes)
    val dTables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet
    mTables.map(t => {
      if (dTables.contains(t.tableName)) {
        t.tableName -> Query(t.length).firstOption
      } else {
        t.tableName -> None
      }
    })
  }

  def rebuildDatabase() {
    val mTables = List(m.Seasons, m.Conferences, m.Teams, m.ConferenceAssociations, m.Aliases, m.Games, m.Results, m.Users, m.Quotes)
    val dTables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet
    val toDrop = mTables.filter(t => dTables.contains(t.tableName)).map(_.ddl)
    toDrop match {
      case Nil =>
      case d :: Nil => d.drop
      case d :: ds => ds.foldLeft(d)(_ ++ _).drop
    }
    mTables.map(_.ddl).reduceLeft(_ ++ _).create
  }


  def scrapeNcaaTeamsAndConferences(data:List[(String, Team)]) = {
    val conferences: List[Conference] = data.map(_._1).toSet.map((c:String) => Conference(0, ScrapingUtil.nameToKey(c), c, c, None, None, None)).toList
    val teams: List[Team] = data.map(_._2)
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

  def upsertTeamListByKey(teamData: List[Team]) {
    teamData.foreach((t: Team) => {
      if (t.id > 0) {
        teamDao.find(t.key) match {
          case Some(u) => teamDao.update(t)
          case None => teamDao.insert(t)
        }
      } else {
        teamDao.insert(t)
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
