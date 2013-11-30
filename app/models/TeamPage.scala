package models

import org.saddle.Series

case class TeamPage(
                     team: Team,
                     conference: Conference,
                     season: Season,
                     isCurrentSeason: Boolean,
                     schedule: List[ScheduleLine],
                     results: List[ResultLine],
                     conferenceStandings: ConferenceStandings,
                     currentRecords: List[(String, Record)],
                     seasonRecords: List[(Season, Conference, Record, Record)],
                     stats: Map[Statistic, Series[String, Double]]
                     ) {

}
