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
import analysis.ModelRecord
import org.saddle.scalar.Scalar
import org.saddle.stats.RankTie


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
                  "key" -> stat.key,
                  "shortFormat" -> stat.shortFormat,
                  "data" -> jsonData(stat, frame)
                )
              )
            )

            case None => Ok(toJson(Map("status" -> "empty")))
          }
      }
  }

  def jsonData(stat:Statistic, frame: Frame[LocalDate, Team, Double]): JsArray = {
    JsArray(
      for (dt <- frame.rowIx.toSeq.reverse) yield {
        val row: Series[ Team, Double] = frame.first(dt)
        jsonByDate(stat, dt, row)
      }
    )
  }

  def jsonByDate(stat:Statistic, dt: LocalDate, row: Series[Team, Double]): JsObject = {
    val observations: IndexedSeq[JsObject] = row.index.toSeq.map(t => {
      val ix: Int = row.index.getFirst(t)
      val value = row.at(ix)
      val rank = row.rank(RankTie.Min, !stat.higherIsBetter).at(ix)
      val z = value.map(x => (x - row.mean) / row.stdev)
      val percentile = row.rank(RankTie.Max, !stat.higherIsBetter).at(ix).map(rk => (row.length - rk) / row.length)
      (t, value, rank, z, percentile)
    }).filter(tup => !(tup._2.isNA || tup._3.isNA || tup._4.isNA || tup._5.isNA)).sortBy(_._3).map {
      case (t: Team, value: Scalar[Double], rank: Scalar[Double], z: Scalar[Double], percentile: Scalar[Double]) => {
        val backgroundColor: String = t.secondaryColor.getOrElse("#ddd")
        Json.obj(
          "team" -> Json.obj("key" -> t.key, "name" -> t.name, "background" -> backgroundColor),
          "value" -> value.get,
          "rank" -> rank.get,
          "z" -> z.get,
          "percentile" -> percentile.get
        )
      }
    }


    Json.obj(
      "date" -> dt,
      "mean" -> row.mean,
      "stdDev" -> row.stdev,
      "max" -> row.max.get,
      "min" -> row.min.get,
      "median" -> row.median,
      "observations" -> JsArray(observations)
    )
  }
}

