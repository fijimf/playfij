package models

import org.joda.time.LocalDate

case class KeyedValue(
                        id: Long,
                        key: String,
                        domainId: Long,
                        numericValue: Option[Double],
                        textValue: Option[String]
                        )