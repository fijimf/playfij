package models

import org.joda.time.LocalDate

case class ScheduleDao(m: Model) {

  import m.profile.simple._

  def schedule(): Schedule = {
    val teams: List[Team] = TeamDao(m).list
    val seasons: List[Season] = SeasonDao(m).list
    val conferences: List[Conference] = ConferenceDao(m).list

    val gameData: Map[Season, List[(Season, Game, Team, Option[Int], Team, Option[Int])]] = (for {(g, r) <- m.Games leftJoin m.Results on (_.id === _.gameId)
                                                                                                  h <- g.homeTeamFk
                                                                                                  a <- g.awayTeamFk
                                                                                                  s <- g.seasonFk
                                                                                                  g <- g} yield (s, g, h, r.homeScore.?, a, r.awayScore.?)).list().groupBy(_._1)
    val assnData: Map[Season, List[(Season, Team, Conference)]] = (for {ca <- m.ConferenceAssociations
                                                                        s <- m.Seasons if s.id === ca.seasonId
                                                                        t <- m.Teams if t.id === ca.teamId
                                                                        c <- m.Conferences if c.id === ca.conferenceId

    } yield (s, t, c)).list.groupBy(_._1)

    Schedule(teams, seasons, conferences, gameData, assnData)
  }
}

case class Schedule(teams: List[Team], seasons: List[Season], conferences: List[Conference], gameData: Map[Season, List[(Season, Game, Team, Option[Int], Team, Option[Int])]], assnData: Map[Season, List[(Season, Team, Conference)]]) {
  val teamToConf: Map[Season, Map[Team, Conference]] = assnData.mapValues(_.map(t => (t._2, t._3)).toMap)

  val confToTeam: Map[Season, Map[Conference, List[Team]]] = assnData.mapValues(_.map(t => (t._3, t._2)).groupBy(_._1).mapValues(_.map(_._2)))


  def season(d: LocalDate): Option[Season] = {
    seasons.find(s => d.isAfter(s.from) && d.isBefore(s.to))
  }

  def currentSeason: Season = {
    season(new LocalDate).getOrElse(seasons.sortBy(_.to).reverse.head)
  }

  def generateRecords(fs: Map[String, (Game, Team) => Boolean]): Map[(String, String, String), Record] = {
    val z: Map[(String, String, String), Record] = Map.empty[(String, String, String), Record].withDefaultValue(Record())
    gameData.values.flatten.foldLeft(z)((map: Map[(String, String, String), Record], data: (Season, Game, Team, Option[Int], Team, Option[Int])) => {
      val (season, game, homeTeam, homeScore, awayTeam, awayScore) = data
      val (winner, loser) = (homeScore, awayScore) match {
        case (Some(h), Some(a)) if (h > a) => (Some(homeTeam), Some(awayTeam))
        case (Some(h), Some(a)) if (h < a) => (Some(awayTeam), Some(homeTeam))
        case _ => (None, None)
      }
      fs.foldLeft() {
        case (label: String, function: ((Game, Team) => Boolean)) => {
          if (winner.isDefined && function(game, winner.get)) {
            val compoundKey = (winner.get.key, season.key, label)
            z+ (compoundKey -> z(compoundKey).++)
          }
        }
      }
    })
  }
}

trait Streak {
  def ++():Streak
  def --():Streak
}

case class WinStreak(n:Int) extends Streak {
  def ++(): Streak = WinStreak(n+1)

  def --(): Streak = LossStreak(1)
}

case class LossStreak(n:Int) extends Streak {
  def ++(): Streak = WinStreak(1)

  def --(): Streak = LossStreak(n+1)
}

case object emptyStreak extends Streak {
  def ++(): Streak = WinStreak(1)
  def --(): Streak = LossStreak(1)
}

case class Record(wins:Int=0, losses:Int=0, streak:Streak=emptyStreak){
  def ++(): Record = Record(wins+1, losses, streak++)
  def --(): Record = Record(wins, losses+1, streak--)
}
