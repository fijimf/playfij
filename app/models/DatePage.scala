package models

import org.joda.time.LocalDate
import analysis.predictors.SingleStatConditioner
import controllers.routes

case class DatePage(
                     date: LocalDate,
                     prevDate:  LocalDate,
                     nextDate:LocalDate,
                     results:List[ScheduleData],
                     games:List[ScheduleData],
                     teamData:Map[Team, TeamSummary],
                     predictors:List[(Statistic, SingleStatConditioner)]
                     ) {
             def nextLink = routes.Schedule.date(nextDate.getYear, nextDate.getMonthOfYear, nextDate.getDayOfMonth)
             def prevLink = routes.Schedule.date(prevDate.getYear, prevDate.getMonthOfYear, prevDate.getDayOfMonth)
}
