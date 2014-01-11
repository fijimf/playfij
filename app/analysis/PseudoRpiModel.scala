package analysis

import models.{Team, Statistic, ScheduleData}
import org.joda.time.LocalDate
import scala.Predef._
import models.ScheduleData
import models.Statistic
import models.Team

class PseudoRpiModel extends ComputableModel {
  def key: String = "pseudo-rpi"

  def statistics: Map[String, Statistic] = ???

  def computeSeason(data: List[ScheduleData]): ModelResult = ???

}