package controllers

import play.mvc.Controller
import securesocial.core.SecureSocial
import play.api.Logger
import models._
import models.ScheduleData
import models.ScheduleDao
import analysis.junkyard.SpreadDist
import org.joda.time.LocalDate
import analysis.frame.{Population, Frame}
import org.apache.commons.math.stat.descriptive.DescriptiveStatistics

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
              val pop: Population[Team, Double] = frame.population(sd.game.date)
              val homeScore = pop.value(sd.homeTeam)
              val awayScore = pop.value(sd.awayTeam)
              if (homeScore.isEmpty || awayScore.isEmpty){
                None
              } else {
                if (math.round(awayScore.get) == math.round(homeScore.get)) {
                  Some(sd.copy(result = sd.result.map(_.copy(homeScore = 1 + math.round(homeScore.get).toInt, awayScore = math.round(awayScore.get).toInt))))
                } else {
                  Some(sd.copy(result = sd.result.map(_.copy(homeScore = math.round(homeScore.get).toInt, awayScore = math.round(awayScore.get).toInt))))
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
    val spreads: List[Double] = data.map(o => math.abs(o.result.get.homeScore - o.result.get.awayScore).toDouble)
    val counts: Map[Double, Int] = spreads.groupBy((x: Double) => x).mapValues(_.size).withDefault((x: Double) => 0)
    val hiCount = counts.map(_._2).max
    val mode = (counts.filter(_._2 == hiCount).map(_._1).toList, hiCount)
    val vec: DescriptiveStatistics = new DescriptiveStatistics(spreads.toArray[Double])
    val all = SpreadDist(name, vec.getN, vec.getMean, vec.getStandardDeviation, vec.getSkewness, vec.getKurtosis, 100.0 * counts(1) / vec.getN, vec.getMin, vec.getPercentile(0.25), vec.getPercentile(0.50), vec.getPercentile(0.75), vec.getPercentile(0.95), vec.getPercentile(0.99), vec.getMax, (List.empty[Int], 0))
    all
  }
}
