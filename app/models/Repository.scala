package models

import scala.slick.driver.ExtendedProfile
import scala.slick.session.Database._
import play.api.db.slick.Profile
import scala.slick.jdbc.meta.MTable
import play.api.Logger
import scala.slick.lifted.DDL
import scraping.util.ScrapingUtil

class Repository(p: ExtendedProfile) {
  val logger: Logger = Logger("Repository")

  val m = new Model() {
    val profile = p
  }

  val modelTables = List(
    m.Seasons,
    m.Conferences,
    m.Teams,
    m.ConferenceAssociations,
    m.Aliases,
    m.Games,
    m.Results,
    m.Users,
    m.Quotes
  )

  def checkDatabase(): List[(String, Option[Int])] = {
    import m.profile.simple._
    val dTables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet
    modelTables.map(t => {
      if (dTables.contains(t.tableName)) {
        t.tableName -> Query(t.length).firstOption
      } else {
        t.tableName -> None
      }
    })
  }

  def rebuildDatabase() {
    import m.profile.simple._

    logger.warn("Rebuilding database")
    logger.info("Schema tables: " + modelTables.map(_.tableName).mkString(","))
    val dTables: Set[String] = MTable.getTables.mapResult(_.name.name).list.toSet
    logger.info("Database tables: " + dTables.mkString(","))
    val tablesToDrop = modelTables.filter(t => dTables.contains(t.tableName))
    val toDrop = tablesToDrop.map(_.ddl)
    logger.info("DDL's dropping:" + toDrop.map(_.dropStatements.mkString("----------\n", "\n", "-----------\n")))
    toDrop match {
      case Nil =>
      case d :: Nil => d.drop
      case d :: ds => ds.foldLeft(d)(_ ++ _).drop
    }
    modelTables.foreach(t => logger.info(t.ddl.createStatements.mkString("\n")))
    val left: DDL = modelTables.map(_.ddl).reduceLeft(_ ++ _)
    left.create
  }


  def scrapeNcaaTeamsAndConferences(data: List[(String, Team)]) = {
    val conferences: List[Conference] = data.map(_._1).toSet.map((c: String) => Conference(0, ScrapingUtil.nameToKey(c), c, c, None, None, None)).toList
    val teams: List[Team] = data.map(_._2)
    logger.info("Loaded team data")
    upsertConferenceListByKey(conferences)
    upsertTeamListByKey(teams)
    val keyList: List[(String, String)] = data.map(tup => (tup._2.key, ScrapingUtil.nameToKey(tup._1)))
    updateAllSeasonConferences(keyList)
  }

  def updateAllSeasonConferences(keyList: List[(String, String)]) {
    import m.profile.simple._
    val seasons: List[Season] = Query(m.Seasons).list()
    keyList.foreach {
      case (teamKey: String, confKey: String) =>
        (for (t <- m.Teams; if (t.key === teamKey);
              c <- m.Conferences; if (c.key === confKey)) yield (t.id, c.id)).foreach {
          case (teamId: Long, confId: Long) => {
            seasons.foreach(s => {
              m.ConferenceAssociations.autoInc.insert(s.id, confId, teamId)
            })
          }
        }
    }
  }

  def upsertConferenceListByKey(conferences: List[Conference]) {
    val conferenceDao: ConferenceDao = ConferenceDao(m)
    conferences.foreach(c => {
      conferenceDao.find(c.key) match {
        case Some(d) => conferenceDao.update(c)
        case None => conferenceDao.insert(c)
      }
    })
  }

  def upsertTeamListByKey(teamData: List[Team]) {
    val teamDao: TeamDao = TeamDao(m)
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
