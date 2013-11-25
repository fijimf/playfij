package analysis

import models.ScheduleData
import org.joda.time.LocalDate

trait AccumulativeStatistic extends {
  def key: String

  def accumulate(obs: ScheduleData, data: Map[LocalDate, Map[Long, Double]]): Map[LocalDate, Map[Long, Double]] = {
    data + (obs.game.date -> accumulateDate(obs, data(obs.game.date)))
  }

  def accumulateDate(obs: ScheduleData, data: Map[Long, Double]): Map[Long, Double]
}