package analysis

import org.joda.time.LocalDate

case class ModelRunRequest(fromDate: Option[LocalDate] = None, toDate: Option[LocalDate] = None)
