package models

import org.joda.time.LocalDate
import play.api.cache.Cache
import org.saddle.{Frame, Series}
import org.saddle.scalar.Scalar
import org.saddle.stats.RankTie
import analysis.ModelRecord
import java.util.{Date, IllegalFormatConversionException}
import play.api.Logger
import scala.slick.lifted

case class ScheduleDao(m: Model) {
  val SCHEDULE_DATA_CACHE_KEY: String = "!game-data"
  val STAT_DATES_CACHE_KEY: String = "!stat-date"
  val TEAM_MAP_CACHE_KEY: String = "!team-map"
  val STAT_MAP_CACHE_KEY: String = "!stat-map"
  val CONFERENCE_MAP_CACHE_KEY: String="!conf-map"

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

  def teamsPage()(implicit s: scala.slick.session.Session): Option[TeamsPage] = currentSeason.map(season => {
    val games: List[ScheduleData] = loadScheduleData
    TeamsPage(conferenceMap.values.map(conf => loadConference(games, conf, season)).toList.sortBy(_.conference.name))
  })

  def teamPage(teamKey: String)(implicit s: scala.slick.session.Session): Option[TeamPage] = currentSeason.flatMap(season => teamPage(teamKey, season.key))

  def teamPage(teamKey: String, seasonKey: String)(implicit s: scala.slick.session.Session): Option[TeamPage] = {
    Cache.getOrElse[Option[TeamPage]](teamKey + ":" + seasonKey, 900) {
      (for {team <- teamQuery if team.key === teamKey
            season <- seasonQuery if season.key === seasonKey
            assoc <- assocQuery if assoc.seasonId === season.id && assoc.teamId === team.id
            conference <- conferenceQuery if conference.id === assoc.conferenceId
      } yield {
        (team, season, conference)
      }).firstOption.flatMap {
        case (team: Team, season: Season, conference: Conference) => buildPage(team, season, conference, currentSeason.exists(_.key == seasonKey))
      }
    }
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

  def getStatDates(date: LocalDate)(implicit s: scala.slick.session.Session): LocalDate = {
    Cache.getOrElse[LocalDate](STAT_DATES_CACHE_KEY, 3600) {
      Query(m.Observations.map(_.date).max).first().getOrElse(new LocalDate())
    }

  }

  def cleanString(x: Scalar[Double], format: String = "%5.2f"): String = {
    if (x.isNA) {
      "N/A"
    } else {
      try {
        format.format(x.get)
      }
      catch {
        case ex: IllegalFormatConversionException => if (ex.getConversion == 'd') {
          "%.0f".format(x.get)
        } else {
          "%f".format(x.get)
        }
      }
    }
  }

  def buildPage(team: Team, season: Season, conference: Conference, isCurrentSeason: Boolean)(implicit s: scala.slick.session.Session): Option[TeamPage] = {

    val games: List[ScheduleData] = loadScheduleData

    val results: List[ResultLine] = loadResults(games.filter(sd => sd.isSameSeason(season) && sd.hasTeam(team) && sd.result.isDefined), team)
    val schedule: List[ScheduleLine] = loadSchedule(games.filter(sd => sd.isSameSeason(season) && sd.hasTeam(team) && sd.result.isEmpty), team)
    val standings: ConferenceStandings = loadConference(games, conference, season)
    val currentRecords = loadCurrentRecords(season, team, games.filter(sd => sd.isSameSeason(season) && sd.hasTeam(team) && sd.result.isDefined))
    val seasonRecords = loadSeasonRecords(team, games)
    val seriesMap: List[(Statistic, Series[Team, Double])] = loadStats(season.to)
    val stats: List[ModelRecord] = seriesMap.map {
      case (stat: Statistic, ser: Series[Team, Double]) => {
        val ix: Int = ser.index.getFirst(team)
        val value: Scalar[Double] = ser.at(ix)
        val rank: Scalar[Double] = ser.rank(RankTie.Max, !stat.higherIsBetter).at(ix)
        val z: Scalar[Double] = value.map(x => (x - ser.mean) / ser.stdev)
        ModelRecord(stat.name, cleanString(value, stat.longFormat), cleanString(rank, "%.0f"), cleanString(z, "%4.2f"), stat.displayOrder)
      }
    }
    Some(TeamPage(team, conference, season, isCurrentSeason, schedule, results, standings, currentRecords, seasonRecords, stats))
  }


  def loadScheduleData(implicit s: scala.slick.session.Session): List[ScheduleData] = {
    Cache.getOrElse[List[ScheduleData]](SCHEDULE_DATA_CACHE_KEY, 3600) {
      scheduleData.list.map(ScheduleData.tupled)
    }
  }

  def loadCurrentRecords(season: Season, team: Team, data: List[ScheduleData])(implicit s: scala.slick.session.Session) = {

    val seasonRecord: RecordGenerator = SeasonRecord(season)
    List(
      "Overall" -> seasonRecord(team, data),
      "Conference" -> (seasonRecord + ConferenceRecord)(team, data),
      "Non-Conference" -> (seasonRecord + NonConferenceRecord)(team, data),
      "Home" -> (seasonRecord + HomeRecord(team))(team, data),
      "Away" -> (seasonRecord + AwayRecord(team))(team, data),
      "Neutral" -> (seasonRecord + NeutralRecord)(team, data),
      "Last 10" -> (seasonRecord + LastNRecord(10))(team, data),
      "Last 5" -> (seasonRecord + LastNRecord(5))(team, data),
      "November" -> (seasonRecord + MonthRecord(11))(team, data),
      "December" -> (seasonRecord + MonthRecord(12))(team, data),
      "January" -> (seasonRecord + MonthRecord(1))(team, data),
      "February" -> (seasonRecord + MonthRecord(2))(team, data),
      "March" -> (seasonRecord + MonthRecord(3))(team, data),
      "<3 pt Margin" -> (seasonRecord + LessThanMarginRecord(3))(team, data)
    )
  }

  def loadSeasonRecords(team: Team, data: List[ScheduleData]) = {
    val confMap: Map[Season, Conference] = data.filter(_.hasTeam(team)).foldLeft(Map.empty[Season, Conference])((map: Map[Season, Conference], data: ScheduleData) => {
      if (data.homeTeam == team) {
        map + (data.season -> data.homeConference)
      } else {
        map + (data.season -> data.awayConference)
      }
    })
    (for (season <- confMap.keys;
          conference <- confMap.get(season)) yield {
      val seasonRecord: RecordGenerator = SeasonRecord(season)
      (season, conference, seasonRecord(team, data), (seasonRecord + ConferenceRecord)(team, data))
    }).toList
  }


  def loadResults(gameData: List[ScheduleData], team: Team): List[ResultLine] = {
    gameData.map(d => {
      if (team == d.homeTeam) {
        if (d.result.get.homeScore > d.result.get.awayScore) {
          ResultLine(d.game.date, d.awayTeam, "vs", "W", "%d - %d".format(d.result.get.homeScore, d.result.get.awayScore))
        } else {
          ResultLine(d.game.date, d.awayTeam, "vs", "L", "%d - %d".format(d.result.get.homeScore, d.result.get.awayScore))
        }
      } else {
        if (d.result.get.awayScore > d.result.get.homeScore) {
          ResultLine(d.game.date, d.homeTeam, "at", "W", "%d - %d".format(d.result.get.awayScore, d.result.get.homeScore))
        } else {
          ResultLine(d.game.date, d.homeTeam, "at", "L", "%d - %d".format(d.result.get.awayScore, d.result.get.homeScore))
        }
      }
    }).sortBy(_.date.toDate)
  }

  def loadSchedule(gameData: List[ScheduleData], team: Team): List[ScheduleLine] = {
    gameData.map(sd => {
      if (team == sd.homeTeam) {
        ScheduleLine(sd.game.date, sd.awayTeam, "vs")
      } else {
        ScheduleLine(sd.game.date, sd.homeTeam, "at")
      }
    }).sortBy(_.date.toDate)
  }

  def season(d: LocalDate)(implicit s: scala.slick.session.Session): Option[Season] = {
    seasonQuery.list().find(season => season.from.isBefore(d) && season.to.isAfter(d))
  }

  def currentSeason(implicit s: scala.slick.session.Session): Option[Season] = {
    season(new LocalDate).orElse(seasonQuery.list().sortBy(_.to.toDate).reverse.headOption)
  }

  def loadConference(gameData: List[ScheduleData], conference: Conference, season: Season)(implicit s: scala.slick.session.Session): ConferenceStandings = {
    val teams: List[Team] = (for {assoc <- assocQuery if assoc.seasonId === season.id && assoc.conferenceId === conference.id
                                  team <- teamQuery if team.id === assoc.teamId} yield team).list()
    ConferenceStandings.createConferenceStandings(season, conference, teams, gameData)
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
        case (team: Team, conference: Conference) => {
          team.name.toLowerCase.contains(q.toLowerCase) ||
            team.longName.toLowerCase.contains(q.toLowerCase) ||
            team.nickname.toLowerCase.contains(q.toLowerCase) ||
            conference.name.toLowerCase.contains(q.toLowerCase)
        }
      }
    }).getOrElse(List.empty[(Team, Conference)])
  }

  def datePage(date: LocalDate)(implicit s: scala.slick.session.Session): DatePage = {
    DatePage(date, date.minusDays(1), date.plusDays(1), loadScheduleData.filter(d => d.game.date == date && d.result.isDefined), loadScheduleData.filter(d => d.game.date == date && d.result.isEmpty))
  }
}





