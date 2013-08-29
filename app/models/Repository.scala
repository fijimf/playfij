package models

import scala.slick.driver.{ExtendedProfile}
import scala.slick.lifted.DDL
import scala.slick.session.Database._
import play.api.db.slick.Profile

class Repository(p: ExtendedProfile) extends SeasonDao with ConferenceDao with TeamDao with GameDao with ResultDao with Profile {

  val profile = p

  import profile._

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
}