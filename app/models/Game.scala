package models

import org.joda.time.{LocalDate, DateMidnight}
import util.Mappers._
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile

case class Game(id: Long, seasonId: Long, homeTeamId: Long, awayTeamId: Long, date: LocalDate, location: Option[String], isNeutralSite: Boolean) {
  require(homeTeamId != awayTeamId)
}


case class GameDao(model: Model) {

  import model._
  import model.profile.simple._


}

