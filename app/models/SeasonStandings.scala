package models


case class SeasonStandings(conferenceStandings: List[ConferenceStandings])

object SeasonStandings {
  def apply(season:Season, conferenceMap:Map[Long, Conference], teamsForConference:(Season,Conference)=>List[Team], games:List[ScheduleData]):SeasonStandings = SeasonStandings(conferenceMap.values.map(conf => {
    ConferenceStandings.createConferenceStandings(season, conf, teamsForConference(season, conf), games)
  }).toList.sortBy(_.conference.name))

}
