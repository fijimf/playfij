package models

import org.joda.time.LocalDate
import play.api.cache.Cache

case class ScheduleDao(m: Model) {
  import play.api.Play.current
  import m.profile.simple._

  val seasonQuery = for (season <- m.Seasons) yield season
  val teamQuery = for (team <- m.Teams) yield team
  val assocQuery = for (assoc <- m.ConferenceAssociations) yield assoc
  val conferenceQuery = for (conf <- m.Conferences) yield conf
  val gameQuery = for (game <- m.Games) yield game
  val resultQuery = for (result <- m.Results) yield result
  val gameResultQuery = for {(game, result) <- m.Games leftJoin m.Results on (_.id === _.gameId)} yield (game, result.maybe)

  def teamPage(teamKey: String)(implicit s: scala.slick.session.Session): Option[TeamPage] = currentSeason.flatMap(season => teamPage(teamKey, season.key))

  def teamPage(teamKey: String, seasonKey: String)(implicit s: scala.slick.session.Session): Option[TeamPage] = {
    Cache.getOrElse[Option[TeamPage]](teamKey + ":" + seasonKey) {
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


  def buildPage(team: Team, season: Season, conference: Conference, isCurrentSeason: Boolean)(implicit s: scala.slick.session.Session): Option[TeamPage] = {
    val games: List[ScheduleData] = Cache.getOrElse[List[ScheduleData]]("!game-data", 3600){
      scheduleData.list.map(ScheduleData.tupled)
    }

    val results: List[ResultLine] = loadResults(games.filter(sd => sd.isSameSeason(season) && sd.hasTeam(team) && sd.result.isDefined), team)
    val schedule: List[ScheduleLine] = loadSchedule(games.filter(sd => sd.isSameSeason(season) && sd.hasTeam(team) && sd.result.isEmpty), team)
    val standings: ConferenceStandings = loadConference(games, conference, season)
    val currentRecords = loadCurrentRecords(season, team, games.filter(sd => sd.isSameSeason(season) && sd.hasTeam(team) && sd.result.isDefined))
    val seasonRecords = loadSeasonRecords(team, games)
    Some(TeamPage(team, conference, season, isCurrentSeason, schedule, results, standings, currentRecords, seasonRecords))
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

}




