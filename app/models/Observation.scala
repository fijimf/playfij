package models

import org.joda.time.LocalDate

case class Observation(
                        id: Long,
                        date: LocalDate,
                        domainId: Long,
                        statisticId: Long,
                        value: Double
                        )