package controllers

import play.api.mvc._
import securesocial.core.SecureSocial
import models._
import org.saddle.Frame
import org.joda.time.LocalDate
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.libs.json.Json._
import scala.Some
import models.Statistic
import models.ScheduleDao
import scala.Some

object Game extends Controller with SecureSocial {

  import play.api.Play.current

  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)

  def games = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val games: List[ScheduleData] = scheduleDao.loadScheduleData
          Ok(
            Json.obj(
              "status" -> "ok",
              "games" -> jsonGameData(games.filter(_.result.isDefined))
            )
          )
      }
  }

  def scatter = UserAwareAction {
    implicit request =>
      Ok(views.html.gamesScatter("blah"))
  }

  def jsonGameData(games: List[ScheduleData]): JsArray = {
    JsArray(games.map { sd=>
          Json.obj(
            "dt" -> sd.game.date.toString,
            "ht" -> sd.homeTeam.key,
            "hs" -> sd.result.get.homeScore,
            "at" -> sd.awayTeam.key,
            "as" -> sd.result.get.awayScore
          )}
        )

  }

  def list = TODO

  def submit = TODO

  def create = TODO

  def view(id: String) = TODO

  def edit(id: String) = TODO

  def delete = TODO

}
