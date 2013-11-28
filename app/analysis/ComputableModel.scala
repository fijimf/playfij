package analysis

import models._
import org.joda.time.LocalDate
import play.api.Logger


trait ComputableModel {
  val logger = Logger("ComputableModel")
  type ModelResult = Map[String, StatisticalResult]
  type StatisticalResult = Map[LocalDate, Observations]
  type Observations = Map[Long, Double]

  def key: String

  def compute(data: List[ScheduleData]) = {
    logger.info("Computing stats for model '%s' using observations from %d games".format(key, data.size))
    data.groupBy(_.season).map(_._2).toList.map(lst => {
      logger.info("Computing by season (%d games)".format(lst.size))
      computeSeason(lst)
    }).flatten.toMap

  }

  def statistics: Map[String, Statistic]

  def computeSeason(data: List[ScheduleData]): ModelResult
}













