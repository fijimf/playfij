package analysis

import org.joda.time.LocalDate
import models.{Statistic, ScheduleData}
//
//class ScoringModel extends ComputableModel {
//  def key = "scoring"
//
//  def statistics: Map[String, Statistic] = List (
//    Statistic(-1,"mean-points-for", "Mean Points For", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1),
//    Statistic(-1,"mean-points-against", "Mean Points Against", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1),
//    Statistic(-1,"mean-points-margin", "Mean Points Margin", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1),
//    Statistic(-1,"mean-points-total", "Mean Points Total", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1),
//    Statistic(-1,"stddev-points-for", "Std Dev Points For", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1),
//    Statistic(-1,"stddev-points-against", "Std Dev Points Agains", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1),
//    Statistic(-1,"stddev-points-margin", "Std Dev Points Margin", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1),
//    Statistic(-1,"stddev-points-total", "Std Dev Points Total", -1, "Team", "%6.3f","%6.3f" , higherIsBetter = true, 1)
//  ).map(s=>s.key->s).toMap
//
//  def computeSeason(data: List[ScheduleData]): ModelResult = {
//
//    data.foldLeft()
//  }
//}
