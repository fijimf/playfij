package models

import org.joda.time.LocalDate

case class ResultLine(date: LocalDate, opp: Team, versusOrAt: String, outcome: String, scores: String)

object ResultLine {
  def apply(gameData: List[ScheduleData], team: Team): List[ResultLine] = {
    gameData.filter(d => d.hasTeam(team) && d.result.isDefined).map(d => {
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
}
