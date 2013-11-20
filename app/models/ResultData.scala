package models

case class ResultData(
                       season: Season,
                       game: Game,
                       homeTeam: Team,
                       awayTeam: Team,
                       homeConference: Conference,
                       awayConference: Conference,
                       result: Result)

case class ScheduleData(
                         season: Season,
                         game: Game,
                         homeTeam: Team,
                         awayTeam: Team,
                         homeConference: Conference,
                         awayConference: Conference,
                         result: Option[Result]) {
  def hasTeam(t: Team): Boolean = homeTeam.key == t.key || awayTeam.key == t.key

  def hasConference(c: Conference): Boolean = homeConference.key == c.key || awayConference.key == c.key

  def isSameSeason(s: Season): Boolean = season.key == s.key

  def isConference: Boolean = homeConference.key == awayConference.key

}
