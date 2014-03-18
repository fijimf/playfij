package analysis.predictors

import models.ScheduleData

trait Categorizer {
  def name:String
  def category(d:ScheduleData):Option[Int]
}
