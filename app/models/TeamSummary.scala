package models

import org.saddle.Series
import analysis.ModelRecord

case class TeamSummary(
                     team: Team,
                     conference: Conference,
                     season: Season,
                     isCurrentSeason: Boolean,
                     schedule: List[ScheduleLine],
                     results: List[ResultLine],
                     conferenceStandings: ConferenceStandings,
                     currentRecords: List[(String, Record)],
                     seasonRecords: List[(Season, Conference, Record, Record)],
                     stats: List[ModelRecord]
                     ) {

}

object TeamSummary {

  import controllers.Util.timed
  def apply( season: Season, isCurrentSeason: Boolean, team: Team, conference: Conference, confTeams:List[Team], games: List[ScheduleData], seriesMap: List[(Statistic, Series[Team, Double])]): Some[TeamSummary] = {
    val results = timed("results"){ResultLine(games.filter(sd => sd.isSameSeason(season)), team)}
    val schedule = timed("schedule"){ScheduleLine(games.filter(sd => sd.isSameSeason(season)), team)}
    val standings = timed("standings"){ConferenceStandings.createConferenceStandings(season, conference, confTeams, games)}
    val currentRecords = timed("currentRecords"){TeamSummary.currentRecords(season, team, games.filter(sd => sd.isSameSeason(season) && sd.hasTeam(team) && sd.result.isDefined))}
    val seasonRecords = timed("seasonRecords"){TeamSummary.seasonRecords(team, games)}
    val stats: List[ModelRecord] = timed("stats") {
      seriesMap.map {
        case (stat: Statistic, ser: Series[Team, Double]) => {
          val ix: Int = ser.index.getFirst(team)
          ModelRecord.fromStatValue(stat, ix, ser)
        }
      }
    }
    Some(TeamSummary(team, conference, season, isCurrentSeason, schedule, results, standings, currentRecords, seasonRecords, stats))
  }


  def currentRecords(season: Season, team: Team, data: List[ScheduleData]) = {

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

  def seasonRecords(team: Team, data: List[ScheduleData]) = {
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


}
