package models

import org.joda.time.LocalDate
import play.api.cache.Cache
import org.saddle.{Frame, Series}
import java.util.Date
import play.api.Logger

case class ScheduleDao(m: Model) {
  val SCHEDULE_DATA_CACHE_KEY: String = "!game-data"
  val STAT_DATES_CACHE_KEY: String = "!stat-date"
  val TEAM_MAP_CACHE_KEY: String = "!team-map"
  val STAT_MAP_CACHE_KEY: String = "!stat-map"
  val CONFERENCE_MAP_CACHE_KEY: String = "!conf-map"

  import play.api.Play.current
  import m.profile.simple._
  import models.util.Mappers._

  val logger = Logger("ScheduleDao")
  val seasonQuery = for (season <- m.Seasons) yield season
  val teamQuery = for (team <- m.Teams) yield team
  val statQuery = for (stat <- m.Statistics) yield stat
  val assocQuery = for (assoc <- m.ConferenceAssociations) yield assoc
  val conferenceQuery = for (conf <- m.Conferences) yield conf
  val gameQuery = for (game <- m.Games) yield game
  val resultQuery = for (result <- m.Results) yield result
  val gameResultQuery = for {(game, result) <- m.Games leftJoin m.Results on (_.id === _.gameId)} yield (game, result.maybe)

  val scheduleData = for {
    (game, result) <- gameResultQuery
    season <- game.seasonFk
    homeTeam <- game.homeTeamFk
    awayTeam <- game.awayTeamFk
    homeAssoc <- assocQuery if homeAssoc.teamId === homeTeam.id && homeAssoc.seasonId === season.id
    awayAssoc <- assocQuery if awayAssoc.teamId === awayTeam.id && awayAssoc.seasonId === season.id
    homeConf <- homeAssoc.conferenceFk
    awayConf <- awayAssoc.conferenceFk
  } yield {
    (season, game, homeTeam, awayTeam, homeConf, awayConf, result)
  }

  def loadScheduleData(implicit s: scala.slick.session.Session): List[ScheduleData] = {
    Cache.getOrElse[List[ScheduleData]](SCHEDULE_DATA_CACHE_KEY, 3600) {
      scheduleData.list.map(ScheduleData.tupled)
    }
  }

  val statData = for {
    observation <- m.Observations
    statistic <- observation.statisticFk
    model <- statistic.modelFk
  } yield {
    (observation, statistic, model)
  }

  val teamStatData = for {
    observation <- m.Observations
    statistic <- observation.statisticFk if statistic.targetDomain === "Team"
    model <- statistic.modelFk
    team <- m.Teams if observation.domainId === team.id
  } yield {
    (observation, statistic, model, team)
  }

  def loadStats(date: LocalDate)(implicit s: scala.slick.session.Session): List[(Statistic, Series[Team, Double])] = {
    logger.info("Starting loadStats: " + new Date().toString)
    val sm = statMap
    val tm = teamMap
    val statDate: LocalDate = getStatDates(date)
    val os = (for {obs <- m.Observations if obs.date === statDate} yield obs).list
    val stats = os.groupBy(o => sm(o.statisticId)).mapValues(lst => Series(lst.map(o => (tm(o.domainId), o.value)): _*)).toList
    logger.info("Finished loadStats: " + new Date().toString)
    stats
  }


  def statPage(statKey: String)(implicit s: scala.slick.session.Session): Option[(Statistic, Frame[LocalDate, Team, Double])] = {
    currentSeason.map(season => {
      val rawData: List[(Statistic, Team, LocalDate, Double)] = (
        for {
          (observation, statistic, model, team) <- teamStatData if statistic.targetDomain === "Team" && statistic.key === statKey && (observation.date >= season.from) && (observation.date <= season.to)
        } yield {
          (statistic, team, observation.date, observation.value)
        }).list()
      val series: Map[Team, Series[LocalDate, Double]] = rawData.groupBy(_._2).mapValues(lst => {
        Series[LocalDate, Double](lst.map(x => (x._3, x._4)): _*)
      })
      val stat = rawData.head._1
      (stat, Frame.apply[LocalDate, Team, Double](series.toList: _*))
    })
  }

  def teamsPage()(implicit s: scala.slick.session.Session): Option[SeasonStandings] = currentSeason.map(season => SeasonStandings(season, conferenceMap, teamsForConference, loadScheduleData))

  def teamSummary(teamKey: String)(implicit s: scala.slick.session.Session): Option[TeamSummary] = currentSeason.flatMap(season => teamSummary(teamKey, season.key))

  def teamSummary(teamKey: String, seasonKey: String)(implicit s: scala.slick.session.Session): Option[TeamSummary] = {
    Cache.getOrElse[Option[TeamSummary]](teamKey + ":" + seasonKey, 900) {
      (for {team <- teamQuery if team.key === teamKey
            season <- seasonQuery if season.key === seasonKey
            assoc <- assocQuery if assoc.seasonId === season.id && assoc.teamId === team.id
            conference <- conferenceQuery if conference.id === assoc.conferenceId
      } yield {
        (team, season, conference)
      }).firstOption.flatMap {
        case (team, season, conference) =>
          val isCurrent = currentSeason.exists(_.key == seasonKey)
          TeamSummary(season, isCurrent, team, conference, teamsForConference(season, conference), loadScheduleData, loadStats(season.to))
      }
    }
  }

  def statMap(implicit s: scala.slick.session.Session): Map[Long, Statistic] = {
    Cache.getOrElse[Map[Long, Statistic]](STAT_MAP_CACHE_KEY, 3600) {
      statQuery.list.map(s => s.id -> s).toMap
    }
  }

  def conferenceMap(implicit s: scala.slick.session.Session): Map[Long, Conference] = {
    Cache.getOrElse[Map[Long, Conference]](CONFERENCE_MAP_CACHE_KEY, 3600) {
      conferenceQuery.list.map(s => s.id -> s).toMap
    }
  }

  def teamMap(implicit s: scala.slick.session.Session): Map[Long, Team] = {
    Cache.getOrElse[Map[Long, Team]](TEAM_MAP_CACHE_KEY, 3600) {
      teamQuery.list.map(t => t.id -> t).toMap
    }
  }

  def getStatDates(WhyAmINotUsed: LocalDate)(implicit s: scala.slick.session.Session): LocalDate = {
    Cache.getOrElse[LocalDate](STAT_DATES_CACHE_KEY, 3600) {
      Query(m.Observations.map(_.date).max).first().getOrElse(new LocalDate())
    }
  }

  def season(d: LocalDate)(implicit s: scala.slick.session.Session): Option[Season] = {
    seasonQuery.list().find(season => season.from.isBefore(d) && season.to.isAfter(d))
  }

  def currentSeason(implicit s: scala.slick.session.Session): Option[Season] = {
    season(new LocalDate).orElse(seasonQuery.list().sortBy(_.to.toDate).reverse.headOption)
  }

  def teamsForConference(season: Season, conference: Conference)(implicit s: scala.slick.session.Session): List[Team] = {
    (for {assoc <- assocQuery if assoc.seasonId === season.id && assoc.conferenceId === conference.id
          team <- teamQuery if team.id === assoc.teamId} yield team).list()
  }

  def search(q: String)(implicit s: scala.slick.session.Session): List[(Team, Conference)] = {
    currentSeason.map(ss => {
      (for {team <- teamQuery
            season <- seasonQuery if season.key === ss.key
            assoc <- assocQuery if assoc.seasonId === season.id && assoc.teamId === team.id
            conference <- conferenceQuery if conference.id === assoc.conferenceId
      } yield {
        (team, conference)
      }).list.filter {
        case (team: Team, conference: Conference) =>
          team.name.toLowerCase.contains(q.toLowerCase) ||
            team.longName.toLowerCase.contains(q.toLowerCase) ||
            team.nickname.toLowerCase.contains(q.toLowerCase) ||
            conference.name.toLowerCase.contains(q.toLowerCase)
      }
    }).getOrElse(List.empty[(Team, Conference)])
  }

  def datePage(date: LocalDate)(implicit s: scala.slick.session.Session): DatePage = {

    val todayData: List[ScheduleData] = loadScheduleData.filter(d => d.game.date == date)
    val teamData: Map[Team, TeamSummary] = (todayData.map(_.homeTeam)++todayData.map(_.awayTeam)).map(t=>t->teamSummary(t.key)).filter(_._2.isDefined).map(t=>t._1->t._2.get).toMap
    DatePage(date, date.minusDays(1), date.plusDays(1), todayData.filter(_.result.isDefined), todayData.filter(_.result.isEmpty), teamData)
  }
}





