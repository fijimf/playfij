package analysis

import models.{Statistic, ScheduleData}

class PseudoRpiModel extends AccumulatorModel {
  def key: String = "pseudo-rpi"

  def statistics: Map[String, Statistic] = ???

  def accumulators: List[AccumulativeStatistic[_]] = List(

  )
}
