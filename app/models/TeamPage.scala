package models

case class TeamPage(
                     team: Team,
                     conference: Conference,
                     season: Season, schedule: List[ScheduleLine],
                     results: List[ResultLine],
                     conferenceStandings: ConferenceStandings,
                     currentRecords:List[(String, Record)],
                     seasonRecords:List[(Season,Conference, Record, Record)]
                     ) {

}
