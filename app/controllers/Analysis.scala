package controllers

import play.mvc.Controller
import securesocial.core.SecureSocial
import play.api.Logger
import models._
import org.saddle.{Frame, Vec, Series}
import analysis.junkyard.SpreadDist
import org.saddle.stats.PctMethod
import models.ScheduleData
import models.ScheduleDao
import analysis.junkyard.SpreadDist
import scala.collection.immutable.Iterable
import org.joda.time.LocalDate

object Analysis extends Controller with SecureSocial {

  import play.api.Play.current

  private val logger = Logger("Statistics")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)

  def spread = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val data: List[ScheduleData] = scheduleDao.loadScheduleData.filter(_.result.isDefined)

          val spreadDists: List[SpreadDist] = createSpreadDists(data)

          val pointPredictor = scheduleDao.statPage("score-predictor")
          val fakeDists = pointPredictor.map(d => {
            val frame: Frame[LocalDate, Team, Double] = d._2
            val fakeData=data.flatMap(sd => {
              val series: Series[Team, Double] = frame.first(sd.game.date)
              val homeScore = series.first(sd.homeTeam)
              val awayScore = series.first(sd.awayTeam)
              if (homeScore.isNA || awayScore.isNA){
                None
              } else {
                if (math.round(awayScore.get) == math.round(homeScore.get)) {
                  Some(sd.copy(result = sd.result.map(_.copy(homeScore = 1 + math.round(homeScore).toInt, awayScore = math.round(awayScore).toInt))))
                } else {
                  Some(sd.copy(result = sd.result.map(_.copy(homeScore = math.round(homeScore).toInt, awayScore = math.round(awayScore).toInt))))
                }
              }
            })
            createSpreadDists(fakeData).map(spread => spread.copy(name = spread.name + " (PP)"))
          })





          Ok(views.html.spreadAnalysis(spreadDists ++ fakeDists.getOrElse(List.empty[SpreadDist])))
      }
  }


  def createSpreadDists(data: List[ScheduleData]): List[SpreadDist] = {
    val all: SpreadDist = createSpreadDist(data, "All games")
    val bySeason: List[SpreadDist] = data.groupBy(_.season).map {
      case (season: Season, sd: List[ScheduleData]) => {
        createSpreadDist(sd, season.season)
      }
    }.toList
    val byMonth: List[SpreadDist] = data.groupBy(_.game.date.toString("MMMM")).map {
      case (month: String, sd: List[ScheduleData]) => {
        createSpreadDist(sd, month)
      }
    }.toList
    val spreadDists: List[SpreadDist] = all :: bySeason ++ byMonth
    spreadDists
  }

  def createSpreadDist(data: List[ScheduleData], name: String): SpreadDist = {
    val spreads: List[Int] = data.map(o => math.abs(o.result.get.homeScore - o.result.get.awayScore))
    val counts: Map[Int, Int] = spreads.groupBy((x: Int) => x).mapValues(_.size).withDefault((x: Int) => 0)
    val hiCount = counts.map(_._2).max
    val mode = (counts.filter(_._2 == hiCount).map(_._1).toList, hiCount)
    val vec: Vec[Int] = Vec(spreads: _*)
    val all = SpreadDist(name, vec.length, vec.mean, vec.stdev, vec.skew, 100.0 * counts(1) / vec.length, vec.min.get, vec.percentile(25.0, PctMethod.NIST), vec.median, vec.percentile(75.0, PctMethod.NIST), vec.percentile(95.0, PctMethod.NIST), vec.percentile(99.0, PctMethod.NIST), vec.max.get, mode)
    all
  }
}
