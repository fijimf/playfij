package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models._
import models.ScheduleDao
import org.joda.time.LocalDate
import analysis.frame.{LowRank, Population, Frame}
import play.api.libs.json.Json._
import play.api.libs.json.{JsArray, JsObject, Json}
import analysis.predictors.{SingleStatFeatureMapper, SpreadCategorizer, Predictor}


object Statistics extends Controller with SecureSocial {

  import play.api.Play.current

  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)

  def createScatterData(analysisSet: List[(Double, Double, Double, Double, Int)]): String = {
    JsArray(analysisSet.map(tup => Json.obj("hv" -> tup._1, "av" -> tup._2, "hz" -> tup._3, "az" -> tup._4, "mg" -> tup._5)).toList).toString()
  }


  def createAnalysisSet(data: List[ScheduleData], frame: Frame[LocalDate, Team, Double]): List[(Double, Double, Double, Double, Int)] = {
    data.filter(_.result.isDefined).map(d => {
      if (frame.ordering.contains(d.game.date)) {
        val pop: Population[Team, Double] = frame.population(d.game.date)
        for (hv <- pop.value(d.homeTeam);
             av <- pop.value(d.awayTeam)
        ) yield {
          (hv, av, (hv - pop.mean) / pop.stdDev, (av - pop.mean) / pop.stdDev, d.result.get.homeScore - d.result.get.awayScore)
        }
      } else {
        None
      }
    }).flatten
  }

  def stat(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val teams: List[Team] = scheduleDao.teamDao.teamMap.values.toList
          val scheduleData: List[ScheduleData] = scheduleDao.loadScheduleData

          scheduleDao.statPage(key).fold(Ok(views.html.resourceNotFound("X", "X")))({
            case (statistic: Statistic, frame: Frame[LocalDate, Team, Double]) => {
              val date = frame.ordering.last
              val jsonSeries = createDescriptiveSeries(frame)
              val analysisSet: List[(Double, Double, Double, Double, Int)] = createAnalysisSet(scheduleData, frame)
              val gameScatter = createScatterData(analysisSet)
              val betas: List[(Double, List[Double])] = createSpreadBetas(statistic.name, scheduleData, frame)
              val pop: Population[Team, Double] = frame.population(date)
              val table: List[StatRow] = frame.ids.map(
                team =>
                  for (value <- pop.value(team);
                       rank <- pop.rank(team, LowRank);
                       zScore <- pop.zScore(team);
                       pctile <- pop.percentile(team)) yield
                    StatRow(team, rank, value, pctile, zScore)

              ).toList.flatten

              Ok(views.html.statEg(statistic, date, pop, table, jsonSeries, gameScatter, betas))
            }
          })

      }
  }

  def createSpreadBetas(name:String, scheduleData: List[ScheduleData], frame: Frame[LocalDate, Team, Double]) = {
    val spreadList = List( -15.0, -10.0, -7.5, -5.0, -2.5, 0.0, 2.5, 5.0, 7.5, 10.0, 15.0)
    val fm = SingleStatFeatureMapper(name, frame, useZ = true)
    spreadList.map(x => {
      val cat = SpreadCategorizer(x)
      x -> Predictor.regress(scheduleData, fm, cat)
    })
  }


  def createDescriptiveSeries(frame: Frame[LocalDate, Team, Double]): String = {
    Json.obj("series" -> JsArray(frame.ordering.map {
      date =>
        val pop: Population[Team, Double] = frame.population(date)
        Json.obj(
          "date" -> date.toString("yyyy-MM-dd"),
          "mean" -> pop.mean,
          "stdDev" -> pop.stdDev,
          "min" -> pop.minimum,
          "med" -> pop.median,
          "max" -> pop.maximum
        )
    }.toList)).toString()
  }

  def stats(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val teams: List[Team] = scheduleDao.teamDao.teamMap.values.toList
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
      for (dt <- frame.ordering.reverse) yield {
        val row: Population[Team, Double] = frame.population(dt)
        jsonByDate(stat, dt, row)
      }
    )
  }

  def jsonByDate(stat: Statistic, dt: LocalDate, row: Population[Team, Double]): JsObject = {
    val observations: IndexedSeq[JsObject] = row.ids.map(t => {
      val value = row.value(t)
      val rank = row.rank(t,LowRank)
      (t, value, rank)
    }).filter(tup => !(tup._2.isEmpty || tup._3.isEmpty)).toIndexedSeq.sortBy(_._3.get).map {
      case (t: Team, value: Option[Double], rank: Option[Double]) => {
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
      "stdDev" -> row.stdDev,
      "max" -> row.maximum,
      "min" -> row.minimum,
      "median" -> row.median,
      "observations" -> JsArray(observations)
    )
  }
}
