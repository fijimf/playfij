package models

import scala.slick.driver.{H2Driver, ExtendedProfile}
import scala.slick.lifted.DDL
import scala.slick.session.Database._

class Repository(p: ExtendedProfile) extends SeasonDao with ConferenceDao with TeamDao with GameDao with ResultDao with Profile {

  val profile = p


  val ddl: DDL = Seasons.ddl ++ Conferences.ddl ++ Teams.ddl ++ Games.ddl ++ Results.ddl

  def createSeason(year: String): Long = {
    Seasons.autoInc.insert(year)
  }

  def createTeam(team: Team) {
    Teams.autoInc.insert(team.key, team.name, team.longName, team.nickname, team.primaryColor, team.secondaryColor, team.officialUrl, team.officialTwitter, team.logoUrl)
  }

  def createConference(conference: Conference) {
    Conferences.autoInc.insert(conference.key, conference.name, conference.shortName, conference.officialUrl, conference.officialTwitter, conference.logoUrl)
  }


  def loadSchedule(key:String):Schedule = {

  }

}

object Junk {
  def main(args: Array[String]) {
    val driver: H2Driver.type = H2Driver
    val repository: Repository = new Repository(driver)
    import driver.simple._

    val db: Database = forURL("jdbc:h2:mem:tests", driver = "org.h2.Driver")
    db withSession {
      (repository.ddl).create

      repository.newSeason("2013")
      repository.listSeasons()

      (repository.ddl.createStatements.foreach(println(_)))
    }

  }
}