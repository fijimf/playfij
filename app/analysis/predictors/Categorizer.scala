package analysis.predictors

import models.ScheduleData

trait Categorizer {
  def category(d:ScheduleData):Option[Int]
}
