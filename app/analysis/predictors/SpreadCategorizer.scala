package analysis.predictors

import models.ScheduleData

case class SpreadCategorizer(x:Double = 0) extends Categorizer {
  override def category(d: ScheduleData): Option[Int] = d.result.map(r => if ((r.homeScore - r.awayScore) > x) 1 else 0)
}
