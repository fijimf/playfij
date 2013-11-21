package analysis

import models._
import org.joda.time.LocalDate
import models.Conference
import models.Team
import models.Season
import models.Game
import org.saddle.Frame


trait ComputableModel {
  type ModelResult = Map[Statistic, Frame[LocalDate, Long, Double]]
  def key:String
  def compute(data:List[ScheduleData]):ModelResult
}


trait WonLostModel extends ComputableModel {
  def key = "won-lost"

  def compute(data:List[ScheduleData]) = {
    val bySeason: List[ List[ScheduleData]] = data.groupBy(_.season).map(_._2).toList
    bySeason.map(lst=>computeSeason(lst))

    Map.empty[Statistic, Frame[LocalDate, Long, Double]]
  }

  def computeSeason(data:List[ScheduleData]) = {
    data.sortBy(_.game.date.toDate)
  }

}
