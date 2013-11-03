package scraping.control

import org.joda.time.LocalDate

case class GameUpdateRequest(url: String = "",
                             doWrite: Boolean = false,
                             doGameInserts: Boolean = true,
                             doGameUpdates: Boolean = true,
                             doGameDeletes: Boolean = false,
                             doResultInserts: Boolean = true,
                             doResultUpdates: Boolean = true,
                             doResultDeletes: Boolean = false,
                             fromDate: Option[LocalDate] = None,
                             toDate: Option[LocalDate] = None)


