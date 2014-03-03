package models

import play.api.cache.Cache
import play.api.Logger
import org.saddle.Series

case class TeamScheduleDao(m: Model) {
  val seasonDao = SeasonDao(m)
  val conferenceDao = ConferenceDao(m)
  val gameDao = GameDao(m)
  val statDao = StatisticDao(m)
  val SCHEDULE_DATA_CACHE_KEY: String = "!game-data"
  val TEAM_MAP_CACHE_KEY: String = "!team-map"
  val STAT_MAP_CACHE_KEY: String = "!stat-map"

  import scala.slick.session.Session
  import play.api.Play.current
  import m.profile.simple._
  import controllers.Util.timed

  val logger = Logger("TeamScheduleDao")
  val teamQuery = for (team <- m.Teams) yield team

  def loadScheduleData(implicit s: Session): List[ScheduleData] = {
    Cache.getOrElse[List[ScheduleData]](SCHEDULE_DATA_CACHE_KEY, 3600) {
      gameDao.scheduleQuery.list.map(ScheduleData.tupled)
    }
  }

  def teamSummary(teamKey: String)(implicit s: Session):Option[TeamSummary] = {
    seasonDao.currentSeason.flatMap(season => teamSummary(teamKey, season.key))
  }

  def teamSummary(keys: List[String])(implicit s: Session):Map[Team,TeamSummary] = {
    seasonDao.currentSeason.map(season => teamSummary(keys, season.key)).getOrElse(Map.empty[Team, TeamSummary])
  }

  def teamSummary(teamKey: String, seasonKey: String)(implicit s: Session) = {
    Cache.getOrElse[Option[TeamSummary]](teamKey + ":" + seasonKey, 900) {
      logger.info("teamSummary cache miss for " + teamKey + ":" + seasonKey)
      (for {team <- teamQuery if team.key === teamKey
            season <- seasonDao.q if season.key === seasonKey
            assoc <- conferenceDao.assocQ if assoc.seasonId === season.id && assoc.teamId === team.id
            conference <- conferenceDao.confQ if conference.id === assoc.conferenceId
      } yield {
        (team, season, conference)
      }).firstOption.flatMap {
        case (team, season, conference) =>
          val isCurrent = seasonDao.currentSeason.exists(_.key == seasonKey)
          TeamSummary(season, isCurrent, team, conference, teamsForConference(season, conference), loadScheduleData, statDao.loadStats(season.to, statMap, teamMap))
      }
    }
  }

  def teamSummary(keys: List[String], seasonKey: String)(implicit s: Session):Map[Team, TeamSummary] = {
    val isCurrent = seasonDao.currentSeason.exists(_.key == seasonKey)
    val (hits,misses) = keys.map(k => k -> Cache.getAs[TeamSummary](k + ":" + seasonKey)).partition(_._2.isDefined)
    val created: List[TeamSummary] = (for {team <- teamQuery
                                       season <- seasonDao.q if season.key === seasonKey
                                       assoc <- conferenceDao.assocQ if assoc.seasonId === season.id && assoc.teamId === team.id
                                       conference <- conferenceDao.confQ if conference.id === assoc.conferenceId
    } yield {
      (team, season, conference)
    }).list.filter(tup=>misses.map(_._1).contains(tup._1.key)).flatMap {
      case (team, season, conference) =>
        val stats: List[(Statistic, Series[Team, Double])] = timed("teamSummary.stats"){statDao.loadStats(season.to, statMap, teamMap)}
        TeamSummary(season, isCurrent, team, conference, teamsForConference(season, conference), loadScheduleData, stats)
    }
    created.foreach(s=>Cache.set(s.team.key, s, 3600))

    (hits.map(t=>t._2.get.team->t._2.get)++created.map(s=>s.team->s)).toMap

  }

  def statMap(implicit s: Session): Map[Long, Statistic] = {
    Cache.getOrElse[Map[Long, Statistic]](STAT_MAP_CACHE_KEY, 3600) {
      statDao.q.list.map(s => s.id -> s).toMap
    }
  }

  def teamMap(implicit s: Session): Map[Long, Team] = {
    Cache.getOrElse[Map[Long, Team]](TEAM_MAP_CACHE_KEY, 3600) {
      teamQuery.list.map(t => t.id -> t).toMap
    }
  }

  def confMap(implicit s: Session): Map[Long, Team] = {
    Cache.getOrElse[Map[Long, Team]](TEAM_MAP_CACHE_KEY, 3600) {
      teamQuery.list.map(t => t.id -> t).toMap
    }
  }

  def teamsForConference(season: Season, conference: Conference)(implicit s: Session): List[Team] = {
    Cache.getOrElse[List[Team]](season.key + ":" + conference.key, 3600) {
      (for {assoc <- conferenceDao.assocQ if assoc.seasonId === season.id && assoc.conferenceId === conference.id
            team <- teamQuery if team.id === assoc.teamId} yield team).list()
    }
  }
}





