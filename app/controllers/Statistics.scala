package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models._
import models.ScheduleDao
import org.joda.time.LocalDate
import org.saddle.{Index, Vec, Series, Frame}
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

  def createScatterData(data: List[ScheduleData], frame: Frame[LocalDate, Team, Double]): String = {
    JsArray(data.filter(_.result.isDefined).map(d => {
      if (frame.rowIx.contains(d.game.date)) {
        val series: Series[Team, Double] = frame.rowAt(frame.rowIx.getFirst(d.game.date.plusDays(-1)))
        if (series.index.contains(d.homeTeam) && series.index.contains(d.awayTeam)) {
          val hv: Scalar[Double] = series.at(series.index.getFirst(d.homeTeam))
          val av: Scalar[Double] = series.at(series.index.getFirst(d.awayTeam))
          val mg = d.result.get.homeScore - d.result.get.homeScore
          Some(hv.get, av.get, mg)
        } else {
          None
        }
      } else {
        None
      }
    }).flatten.map(tup=>Json.obj("hv"->tup._1, "av"->tup._2, "mg"->tup._3)).toList).toString

  }

  def stat(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val teams: List[Team] = scheduleDao.teamMap.values.toList
          val scheduleData: List[ScheduleData] = scheduleDao.loadScheduleData

          scheduleDao.statPage(key).map {
            case (statistic: Statistic, frame: Frame[LocalDate, Team, Double]) => {
              val date = frame.rowIx.last.get
              val jsonSeries = createDescriptiveSeries(frame)
              val gameScatter = createScatterData(scheduleData, frame)
              val values: Series[Team, Double] = frame.rowAt(frame.rowIx.length - 1)
              val table: List[StatRow] = values.toSeq.zipWithIndex.map {
                case ((team, value), i) =>
                  val rank = values.rank(RankTie.Min, !statistic.higherIsBetter).at(i).get.toInt
                  val zScore = (value - values.mean) / values.stdev
                  val pctile = 100.0 * (values.length - rank) / values.length
                  StatRow(team, rank, value, pctile, zScore)
              }.toList.sortBy(_.rank)
              Ok(views.html.statEg(statistic, date, DescriptiveStats(values), table, jsonSeries, gameScatter))
            }
          }.getOrElse(Ok(views.html.resourceNotFound("X", "X")))

      }
  }


  def createDescriptiveSeries(frame: Frame[LocalDate, Team, Double]):String = {
    val jsonSeries = Json.obj("series" -> JsArray(frame.rowIx.toSeq.map {
      date =>
        val descriptiveStats: DescriptiveStats = DescriptiveStats(frame.first(date))
        Json.obj(
          "date" -> date.toString("yyyy-MM-dd"),
          "mean" -> descriptiveStats.mean,
          "stdDev" -> descriptiveStats.stdDev,
          "min" -> descriptiveStats.min,
          "med" -> descriptiveStats.med,
          "max" -> descriptiveStats.max
        )
    }.toList)).toString()
  }

  def stats(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val teams: List[Team] = scheduleDao.teamMap.values.toList
          val observations: Option[(Statistic, Frame[LocalDate, Team, Double])] = scheduleDao.statPage(key)
          observations match {
            case Some((stat, frame)) => Ok(
              Json.obj(
                "status" -> "ok",
                "teams" -> jsonTeamData(teams),
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

  def jsonTeamData(teams: List[Team]): JsObject = {
    val teamData: List[(String, JsValueWrapper)] = teams.map(t => {
      val color1: String = t.primaryColor.getOrElse("#FFF")
      val color2: String = t.secondaryColor.getOrElse("#FFF")
      val obj: JsValueWrapper = Json.obj("name" -> t.name, "c1" -> color1, "c2" -> color2)
      t.key -> obj
    })
    Json.obj(teamData.toSeq: _*)
  }

  def jsonData(stat: Statistic, frame: Frame[LocalDate, Team, Double]): JsArray = {
    JsArray(
      for (dt <- frame.rowIx.toSeq.reverse) yield {
        val row: Series[Team, Double] = frame.first(dt)
        jsonByDate(stat, dt, row)
      }
    )
  }

  def jsonByDate(stat: Statistic, dt: LocalDate, row: Series[Team, Double]): JsObject = {
    val observations: IndexedSeq[JsObject] = row.index.toSeq.map(t => {
      val ix: Int = row.index.getFirst(t)
      val value = row.at(ix)
      val rank = row.rank(RankTie.Min, !stat.higherIsBetter).at(ix)
      (t, value, rank)
    }).filter(tup => !(tup._2.isNA || tup._3.isNA)).sortBy(_._3).map {
      case (t: Team, value: Scalar[Double], rank: Scalar[Double]) => {
        Json.obj(
          "teamKey" -> t.key,
          "value" -> value.get,
          "rank" -> rank.get
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

