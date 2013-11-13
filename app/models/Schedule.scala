package models

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

case class ScheduleDao(m: Model) {

  import m.profile.simple._

  val fmt = DateTimeFormat.forPattern("yyyyMMdd")

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
    }.sortBy(_.date.toDate)
    val schedule: List[ScheduleLine] = gameData.filter(_._4.isEmpty).map {
      case (date: LocalDate, homeTeam: Team, awayTeam: Team, _) => {
        if (team == homeTeam) {
          ScheduleLine(date, awayTeam, "vs")
        } else {
          ScheduleLine(date, homeTeam, "at")
        }
      }
    }.sortBy(_.date.toDate)


    Some(TeamPage(team, conference, season, schedule, results, loadConference(conference.key, season.key)))
  }

  def season(d: LocalDate)(implicit s: scala.slick.session.Session): Option[Season] = {
    seasonQuery.list().find(season => season.from.isBefore(d) && season.to.isAfter(d))
  }

  def currentSeason()(implicit s: scala.slick.session.Session): Option[Season] = {
    season(new LocalDate).orElse(seasonQuery.list().sortBy(_.to.toDate).reverse.headOption)
  }

  def loadConference(conferenceKey:String, seasonKey:String)(implicit s: scala.slick.session.Session):ConferenceStandings = {
    val confData: Map[Conference, List[Team]] = (for {season <- seasonQuery if season.key === seasonKey
                                                    conference <- conferenceQuery if conference.key === conferenceKey
                                                    assoc <- assocQuery if assoc.seasonId === season.id && assoc.conferenceId === conference.id
                                                    team <- teamQuery if team.id === assoc.teamId} yield (conference, team)).list().groupBy(_._1).mapValues(_.map(_._2))
    val (conference, teams) = confData.head
    val confTeamMap: Map[Team, Conference] = (for {season <- seasonQuery if season.key === seasonKey
                                                   assoc <- assocQuery if assoc.seasonId === season.id
                                                   team <- teamQuery if team.id === assoc.teamId
                                                   conference <- conferenceQuery if conference.id === assoc.conferenceId} yield (team, conference)).list().toMap


    val gameData = (for {result <- m.Results
                         game <- result.gameFk
                         homeTeam <- game.homeTeamFk
                         awayTeam <- game.awayTeamFk} yield { (homeTeam, result.homeScore, awayTeam, result.awayScore) }).list()
      .filter(tup=> teams.contains(tup._1) || teams.contains(tup._3))
    val z = Map.empty[Team, (Record, Record)].withDefaultValue(Record(), Record())
    val teamRecords: List[(Team, (Record, Record))] = gameData.foldLeft(z) {
      case (map: Map[Team, (Record, Record)], tuple: (Team, Int, Team, Int)) => {
        val (homeTeam, homeScore, awayTeam, awayScore) = tuple
        (teams.contains(homeTeam), teams.contains(awayTeam)) match {
          case (true, true) => {
            if (homeScore > awayScore) {
              val (hconfRecord, hoverallRecord) = map(homeTeam)
              val hh = (hconfRecord ++, hoverallRecord ++)
              val (aconfRecord, aoverallRecord) = map(awayTeam)
              val aa = (aconfRecord --, aoverallRecord --)
              map + (homeTeam -> hh) + (awayTeam -> aa)
            } else {
              val (hconfRecord, hoverallRecord) = map(homeTeam)
              val hh = (hconfRecord --, hoverallRecord --)
              val (aconfRecord, aoverallRecord) = map(awayTeam)
              val aa = (aconfRecord ++, aoverallRecord ++)
              map + (homeTeam -> hh) + (awayTeam -> aa)
            }
          }
          case (true, false) => {
            if (homeScore > awayScore) {
              val (hconfRecord, hoverallRecord) = map(homeTeam)
              val hh = (hconfRecord, hoverallRecord ++)
              map + (homeTeam -> hh)
            } else {
              val (hconfRecord, hoverallRecord) = map(homeTeam)
              val hh = (hconfRecord, hoverallRecord --)
              map + (homeTeam -> hh)
            }
          }
          case (false, true) => {
            if (homeScore > awayScore) {
              val (aconfRecord, aoverallRecord) = map(awayTeam)
              val aa = (aconfRecord, aoverallRecord --)
              map + (awayTeam -> aa)
            } else {
              val (aconfRecord, aoverallRecord) = map(awayTeam)
              val aa = (aconfRecord, aoverallRecord ++)
              map + (awayTeam -> aa)
            }
          }
          case _ => map
        }

      }
    }.toList

    val sortedTeams: List[(Team, (Record, Record))] = teamRecords.sortWith((t1: (Team, (Record, Record)), t2: (Team, (Record, Record))) => {
      val d = t1._2._1.compareTo(t2._2._1)
      if (d == 0) {
        t1._2._2.compareTo(t2._2._2) > 0
      } else {
        d > 0
      }
    })
    ConferenceStandings(conference,sortedTeams)
  }
}

case class ConferenceStandings(conference:Conference, records:List[(Team, (Record, Record))])

case class ResultLine(date: LocalDate, opp: Team, versusOrAt: String, outcome: String, scores: String)

case class ScheduleLine(date: LocalDate, opp: Team, versusOrAt: String)

case class TeamPage(team: Team, conference: Conference, season: Season, schedule: List[ScheduleLine], results: List[ResultLine], conferenceStandings:ConferenceStandings) {

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

case class Record(wins: Int = 0, losses: Int = 0, streak: Streak = emptyStreak) extends Comparable[Record]{
  def ++ : Record = Record(wins + 1, losses, streak ++)

  def -- : Record = Record(wins, losses + 1, streak --)

  def compareTo(r: Record): Int = {
    val d = (wins - losses) - (r.wins - r.losses)
    if (d == 0) {
      wins - r.wins
    } else {
      d
    }
  }

  override def toString():String = {
    "%d - %d".format(wins,losses)
  }

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