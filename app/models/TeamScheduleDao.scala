package models

import org.joda.time.LocalDate
import play.api.cache.Cache
import org.saddle.{Frame, Series}
import java.util.Date
import play.api.Logger
import controllers.SingleVariableLogisticModel

case class TeamScheduleDao(m: Model) {
  val seasonDao = SeasonDao(m)
  val conferenceDao = ConferenceDao(m)
  val gameDao = GameDao(m)
  val statDao = StatisticDao(m)
  val SCHEDULE_DATA_CACHE_KEY: String = "!game-data"
  val TEAM_MAP_CACHE_KEY: String = "!team-map"
  val STAT_MAP_CACHE_KEY: String = "!stat-map"

  import play.api.Play.current
  import m.profile.simple._

  val logger = Logger("TeamScheduleDao")
  val teamQuery = for (team <- m.Teams) yield team

  def loadScheduleData(implicit s: scala.slick.session.Session): List[ScheduleData] = {
    Cache.getOrElse[List[ScheduleData]](SCHEDULE_DATA_CACHE_KEY, 3600) {
      gameDao.scheduleQuery.list.map(ScheduleData.tupled)
    }
  }

  def teamSummary(teamKey: String)(implicit s: scala.slick.session.Session): Option[TeamSummary] = seasonDao.currentSeason.flatMap(season => teamSummary(teamKey, season.key))

  def teamSummary(teamKey: String, seasonKey: String)(implicit s: scala.slick.session.Session): Option[TeamSummary] = {
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

  def statMap(implicit s: scala.slick.session.Session): Map[Long, Statistic] = {
    Cache.getOrElse[Map[Long, Statistic]](STAT_MAP_CACHE_KEY, 3600) {
      statDao.q.list.map(s => s.id -> s).toMap
    }
  }

  def teamMap(implicit s: scala.slick.session.Session): Map[Long, Team] = {
    Cache.getOrElse[Map[Long, Team]](TEAM_MAP_CACHE_KEY, 3600) {
      teamQuery.list.map(t => t.id -> t).toMap
    }
  }

  def teamsForConference(season: Season, conference: Conference)(implicit s: scala.slick.session.Session): List[Team] = {
    (for {assoc <- conferenceDao.assocQ if assoc.seasonId === season.id && assoc.conferenceId === conference.id
          team <- teamQuery if team.id === assoc.teamId} yield team).list()
  }
}





