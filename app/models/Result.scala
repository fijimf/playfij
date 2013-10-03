package models

import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile

case class Result(id: Long, gameId: Long, homeScore: Int, awayScore: Int, numOts: Int)

case class ResultDao(model: Model) {

  import model._
  import model.profile.simple._


}

