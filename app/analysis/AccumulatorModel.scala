package analysis

import models.ScheduleData
import org.joda.time.LocalDate

trait AccumulatorModel extends ComputableModel {
  def accumulators: List[AccumulativeStatistic]

  def computeSeason(data: List[ScheduleData]) = {
    val z = Map.empty[String, StatisticalResult].withDefault(_ => Map.empty[LocalDate, Observations].withDefault(_ => Map.empty[Long, Double]))
    data.sortBy(_.game.date.toDate).foldLeft(z)((modelResult: ModelResult, obs: ScheduleData) => {
      accumulators.foldLeft(z)((innerResult: ModelResult, statistic: AccumulativeStatistic) => {
        innerResult + (statistic.key -> statistic.accumulate(obs, innerResult(statistic.key)))
      })
    })
  }

}
