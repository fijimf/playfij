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

//    val teamToConf: Map[Season, Map[Team, Conference]] = assnData.mapValues(_.map(t => (t._2, t._3)).toMap)
//
//    val confToTeam: Map[Season, Map[Conference, List[Team]]] = assnData.mapValues(_.map(t => (t._3, t._2)).groupBy(_._1).mapValues(_.map(_._2)))
    Schedule(teams, seasons, conferences, gameData, assnData)
  }
}

case class Schedule(teams: List[Team], seasons: List[Season], conferences: List[Conference], gameData: Map[Season, List[(Season, Game, Team, Option[Int], Team, Option[Int])]],assnData: Map[Season, List[(Season, Team, Conference)]] ) {
  def season(d: LocalDate): Option[Season] = {
    seasons.filter(s => d.isAfter(s.from) && d.isBefore(s.to)).headOption
  }

  def currentSeason: Season = {
    season(new LocalDate).getOrElse(seasons.sortBy(_.to).reverse.head)
  }


}
