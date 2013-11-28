package analysis

import models.ScheduleData
import org.joda.time.LocalDate

trait AccumulativeStatistic extends {
  def key: String

  def accumulate(obs: ScheduleData, data: Map[Long, Double]): Map[Long, Double]
}
