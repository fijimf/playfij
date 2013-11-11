package models

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

case class ScheduleDao(m: Model) {

  import m.profile.simple._

  val fmt = DateTimeFormat.forPattern("yyyyMMdd");

  def schedule(implicit s: scala.slick.session.Session): Schedule = {
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

  val seasonQuery = for (season <- m.Seasons) yield season
  val teamQuery = for (team <- m.Teams) yield team
  val assocQuery = for (assoc <- m.ConferenceAssociations) yield assoc
  val conferenceQuery = for (conf <- m.Conferences) yield conf

  def teamPage(teamKey: String)(implicit s: scala.slick.session.Session): Option[TeamPage] = {
    currentSeason() match {
      case Some(season) => teamPage(teamKey, season.key, new LocalDate().toString(fmt))
      case _ => None
    }
  }

  def teamPage(teamKey: String, yyyymmdd: String)(implicit s: scala.slick.session.Session): Option[TeamPage] = {
    season(fmt.parseLocalDate(yyyymmdd)) match {
      case Some(season) => teamPage(teamKey, season.key, yyyymmdd)
      case _ => None
    }
  }

  def teamPage(teamKey: String, seasonKey: String, yyyymmdd: String)(implicit s: scala.slick.session.Session): Option[TeamPage] = {
    (for {team <- teamQuery if team.key === teamKey
          season <- seasonQuery if season.key === seasonKey
          assoc <- assocQuery if assoc.seasonId === season.id && assoc.teamId === team.id
          conference <- conferenceQuery if conference.id === assoc.conferenceId
    } yield {
      (team, season, conference)
    }).firstOption match {
      case Some((team, season, conference)) => buildPage(team, season, conference, fmt.parseLocalDate(yyyymmdd))
      case _ => None
    }
  }

  def buildPage(team: Team, season: Season, conference: Conference, date: LocalDate)(implicit s: scala.slick.session.Session): Option[TeamPage] = {
    val gameData: List[(LocalDate, Team, Team, Option[Result])] = (for {(game, result) <- m.Games leftJoin m.Results on (_.id === _.gameId) if game.homeTeamId === team.id || game.awayTeamId === team.id
                                                                homeTeam <- game.homeTeamFk
                                                                awayTeam <- game.awayTeamFk
    }
    yield {
      (game.date, homeTeam, awayTeam, result.maybe)
    }).list()
    val results: List[ResultLine] = gameData.filter(_._4.isDefined).map {
      case (date: LocalDate, homeTeam: Team, awayTeam: Team, result: Option[Result]) => {
        if (team == homeTeam) {
          if (result.get.homeScore > result.get.awayScore) {
            ResultLine(date, awayTeam, "vs", "W", "%d - %d".format(result.get.homeScore, result.get.awayScore))
          } else {
            ResultLine(date, awayTeam, "vs", "L", "%d - %d".format(result.get.homeScore, result.get.awayScore))
          }
        } else {
          if (result.get.awayScore > result.get.homeScore) {
            ResultLine(date, homeTeam, "at", "W", "%d - %d".format(result.get.awayScore, result.get.homeScore))
          } else {
            ResultLine(date, homeTeam, "at", "L", "%d - %d".format(result.get.awayScore, result.get.homeScore))
          }
        }
      }
    }
    val schedule: List[ScheduleLine] = gameData.filter(_._4.isEmpty).map {
      case (date: LocalDate, homeTeam: Team, awayTeam: Team, _) => {
        if (team == homeTeam) {
          ScheduleLine(date, awayTeam, "vs")
        } else {
          ScheduleLine(date, homeTeam, "at")
        }
      }
    }

    Some(TeamPage(team, conference, season, schedule, results))
  }

  def season(d: LocalDate)(implicit s: scala.slick.session.Session): Option[Season] = {
    seasonQuery.list().find(season => season.from.isBefore(d) && season.to.isAfter(d))
  }

  def currentSeason()(implicit s: scala.slick.session.Session): Option[Season] = {
    season(new LocalDate).orElse(seasonQuery.list().sortBy(_.to.toDate).reverse.headOption)
  }

}


case class ResultLine(date: LocalDate, opp: Team, versusOrAt: String, outcome: String, scores: String)

case class ScheduleLine(date: LocalDate, opp: Team, versusOrAt: String)

case class TeamPage(team: Team, conference: Conference, season: Season, schedule: List[ScheduleLine], results: List[ResultLine]) {

}

case class Schedule(teams: List[Team], seasons: List[Season], conferences: List[Conference], gameData: Map[Season, List[(Season, Game, Team, Option[Int], Team, Option[Int])]], assnData: Map[Season, List[(Season, Team, Conference)]]) {
  val teamToConf: Map[Season, Map[Team, Conference]] = assnData.mapValues(_.map(t => (t._2, t._3)).toMap)

  val confToTeam: Map[Season, Map[Conference, List[Team]]] = assnData.mapValues(_.map(t => (t._3, t._2)).groupBy(_._1).mapValues(_.map(_._2)))


  def generateRecords(defs: List[RecordDef]): Map[RecordKey, Record] = {
    Map.empty[RecordKey, Record]
  }
}

trait Streak {
  def ++ : Streak

  def -- : Streak
}

case class WinStreak(n: Int) extends Streak {
  def ++ : Streak = WinStreak(n + 1)

  def -- : Streak = LossStreak(1)
}

case class LossStreak(n: Int) extends Streak {
  def ++ : Streak = WinStreak(1)

  def -- : Streak = LossStreak(n + 1)
}

case object emptyStreak extends Streak {
  def ++ : Streak = WinStreak(1)

  def -- : Streak = LossStreak(1)
}

case class Record(wins: Int = 0, losses: Int = 0, streak: Streak = emptyStreak) {
  def ++ : Record = Record(wins + 1, losses, streak ++)

  def -- : Record = Record(wins, losses + 1, streak --)
}

case class RecordKey(teamKey: String, seasonKey: String, typeKey: String)

trait RecordDef {
  def label: String

  def include(g: Game): Boolean
}

case object OverallRecord extends RecordDef {
  def label: String = "Overall"

  def include(g: Game): Boolean = true

}

case class ConferenceTest(teamToConf: Map[Season, Map[Team, Conference]], matchConf: Boolean) extends RecordDef {
  private val idMap: Map[Long, Map[Long, Long]] = teamToConf.map {
    case (season: Season, map: Map[Team, Conference]) => season.id -> map.map {
      case (team: Team, conference: Conference) => (team.id, conference.id)
    }.toMap
  }.toMap

  def label: String = if (matchConf) "Conference" else "Non-Conference"

  def include(g: Game): Boolean = idMap.get(g.seasonId) match {
    case Some(lookup) => matchConf == (lookup.get(g.homeTeamId) == lookup.get(g.awayTeamId))
    case _ => false
  }
}