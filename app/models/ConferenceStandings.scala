package models

case class ConferenceStandings(conference: Conference, records: List[(Team, (Record, Record))]) {
  def getTeam(key: String): Option[(Team, (Record, Record))] = records.find(_._1.key == key)
}

object ConferenceStandings {
  def createConferenceStandings(season: Season, conference: Conference, teams: List[Team], gameData: List[ScheduleData]): ConferenceStandings = {
    val z = teams.map(t => t ->(Record(), Record())).toMap

    def updateConf(map: Map[Team, (Record, Record)], wt: Team, lt: Team): Map[Team, (Record, Record)] = {
      map + (wt -> ((map(wt)._1.addWin(), map(wt)._2.addWin()))) + (lt -> ((map(lt)._1.addLoss(), map(lt)._2.addLoss())))
    }
    def updateNonConfWin(map: Map[Team, (Record, Record)], t: Team): Map[Team, (Record, Record)] = {
      map + (t -> ((map(t)._1, map(t)._2.addWin())))
    }
    def updateNonConfLoss(map: Map[Team, (Record, Record)], t: Team): Map[Team, (Record, Record)] = {
      map + (t -> ((map(t)._1, map(t)._2.addLoss())))
    }
    val teamRecords: List[(Team, (Record, Record))] = gameData.filter(gd => gd.hasConference(conference) && gd.isSameSeason(season) && gd.result.isDefined).foldLeft(z) {
      case (map: Map[Team, (Record, Record)], gd: ScheduleData) => {
        if (gd.isConference) {
          if (gd.result.get.homeScore > gd.result.get.awayScore) {
            updateConf(map, gd.homeTeam, gd.awayTeam)
          } else {
            updateConf(map, gd.awayTeam, gd.homeTeam)
          }
        } else {
          if (gd.homeConference.key == conference.key) {
            if (gd.result.get.homeScore > gd.result.get.awayScore) {
              updateNonConfWin(map, gd.homeTeam)
            } else {
              updateNonConfLoss(map, gd.homeTeam)
            }
          } else {
            if (gd.result.get.homeScore > gd.result.get.awayScore) {
              updateNonConfLoss(map, gd.awayTeam)
            } else {
              updateNonConfWin(map, gd.awayTeam)
            }
          }
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
    ConferenceStandings(conference, sortedTeams)
  }

}
