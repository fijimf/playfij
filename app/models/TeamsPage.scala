package models


case class TeamsPage(conferenceStandings: List[ConferenceStandings]) {

}

object TeamsPage {
  def apply(season:Season, conferenceMap:Map[Long, Conference], teamsForConference:(Season,Conference)=>List[Team], games:List[ScheduleData]):TeamsPage = TeamsPage(conferenceMap.values.map(conf => {
    ConferenceStandings.createConferenceStandings(season, conf, teamsForConference(season, conf), games)
  }).toList.sortBy(_.conference.name))

}
