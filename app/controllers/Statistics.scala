package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models._
import models.ScheduleDao
import org.joda.time.LocalDate
import org.saddle.{Series, Frame}
import play.api.libs.json.Json._
import play.api.libs.json.{JsArray, JsObject, Json}


object Statistics extends Controller with SecureSocial {

  import play.api.Play.current

  private val logger = Logger("Statistics")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)

  def stats(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val data: Option[(Statistic, Frame[LocalDate, Team, Double])] = scheduleDao.statPage(key)
          data match {
            case Some((stat, frame)) => Ok(
              Json.obj(
                "status" -> "ok",
                "statistic" -> Json.obj(
                  "name" -> stat.name,
                  "data" -> jsonData(frame)
                )
              )
            )

            case None => Ok(toJson(Map("status" -> "empty")))
          }
      }
  }

  def jsonData(frame: Frame[LocalDate, Team, Double]): JsArray = {
    JsArray(
      for (dt <- frame.rowIx.toSeq) yield {
        val row: Frame[LocalDate, Team, Double] = frame.row(dt)
        jsonByDate(dt, row)
      }
    )
  }

  def jsonByDate(dt: LocalDate, row: Frame[LocalDate, Team, Double]): JsObject = {
    Json.obj(
      "date" -> dt,
      "observations" -> JsArray(
        row.toSeq.filter(!_._3.isNaN).sortBy(_._3).map(obs => Json.obj(
          "teamKey" -> obs._2.key, "teamName" -> obs._2.name, "value" -> obs._3
        ))
      )
    )
  }
}

