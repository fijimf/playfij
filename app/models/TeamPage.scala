package models

/**
 * Created with IntelliJ IDEA.
 * User: Jim
 * Date: 11/16/13
 * Time: 12:40 AM
 * To change this template use File | Settings | File Templates.
 */
case class TeamPage(team: Team, conference: Conference, season: Season, schedule: List[ScheduleLine], results: List[ResultLine], conferenceStandings: ConferenceStandings) {

}
