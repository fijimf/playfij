package models

import org.joda.time.LocalDate

case class ScheduleLine(date: LocalDate, opp: Team, versusOrAt: String)
