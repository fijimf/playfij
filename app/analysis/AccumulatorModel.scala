package analysis

import models.ScheduleData
import org.joda.time.LocalDate

trait AccumulatorModel extends ComputableModel {
  def accumulators: List[AccumulativeStatistic]

  def computeSeason(data: List[ScheduleData]) = {
    val results: List[ScheduleData] = data.filter(_.result.isDefined).sortBy(_.game.date.toDate)
    logger.info("Filtered results size %d".format(results.size))
    logger.info("First date is %s".format(results.head.game.date))

    val zero = Map.empty[String, (Observations, StatisticalResult)].withDefault(_ =>
      (Map.empty[Long, Double], Map.empty[LocalDate, Observations]))
    results.foldLeft(zero)(handleObs).mapValues(_._2)
  }

  def handleObs(running: Map[String, (Observations, StatisticalResult)], obs: ScheduleData) = {
    accumulators.foldLeft(running)(accumulateStatistic(_, _, obs))
  }

  def accumulateStatistic(running: Map[String, (Observations, StatisticalResult)], statistic: AccumulativeStatistic, obs: ScheduleData) = {
    val (runningTotals, dateResults) = running(statistic.key)
    val updatedTotals = statistic.accumulate(obs, runningTotals)
    running + (statistic.key ->(updatedTotals, dateResults + (obs.game.date -> updatedTotals)))
  }
}
