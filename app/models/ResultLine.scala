package models

import org.joda.time.LocalDate

case class ResultLine(date: LocalDate, opp: Team, versusOrAt: String, outcome: String, scores: String)
