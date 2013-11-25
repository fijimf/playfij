package analysis

import models._
import org.joda.time.LocalDate


trait ComputableModel {
  type ModelResult = Map[String, StatisticalResult]
  type StatisticalResult = Map[LocalDate, Observations]
  type Observations = Map[Long, Double]

  def key: String

  def compute(data: List[ScheduleData]) = {
    data.groupBy(_.season).map(_._2).toList.map(lst => computeSeason(lst)).flatten.toMap
  }

  def computeSeason(data: List[ScheduleData]): ModelResult
}













