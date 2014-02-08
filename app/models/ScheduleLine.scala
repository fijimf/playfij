package models

import org.joda.time.LocalDate

case class ScheduleLine(date: LocalDate, opp: Team, versusOrAt: String)

object ScheduleLine {
  def apply(gameData: List[ScheduleData], team: Team): List[ScheduleLine] = {
    gameData.filter(sd => sd.hasTeam(team) && sd.result.isEmpty).map(sd => {
      if (team == sd.homeTeam) {
        ScheduleLine(sd.game.date, sd.awayTeam, "vs")
      } else {
        ScheduleLine(sd.game.date, sd.homeTeam, "at")
      }
    }).sortBy(_.date.toDate)
  }
}
