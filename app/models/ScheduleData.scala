package models

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

  def isWinner(t: Team) = result.exists(r => (r.homeScore > r.awayScore && t.key == homeTeam.key) || r.awayScore > r.homeScore && t.key == awayTeam.key)

  def isLoser(t: Team) = result.exists(r => (r.homeScore < r.awayScore && t.key == homeTeam.key) || r.awayScore < r.homeScore && t.key == awayTeam.key)

  def winner = result.map(r => if (r.homeScore > r.awayScore) homeTeam else awayTeam)

  def loser = result.map(r => if (r.homeScore < r.awayScore) homeTeam else awayTeam)
}
