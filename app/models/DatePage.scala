package models

import analysis.ModelRecord
import org.joda.time.LocalDate

case class DatePage(
                     date: LocalDate,
                     prevDate:  LocalDate,
                     nextDate:LocalDate,
                     results:List[ScheduleData],
                     games:List[ScheduleData],
                     teamData:Map[Team, TeamSummary]
                     ) {

}
