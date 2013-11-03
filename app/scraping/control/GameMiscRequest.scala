package scraping.control

import org.joda.time.LocalDate

case class GameMiscRequest(url: String = "",
                             doWrite: Boolean = false,
                             doTeamTwitter: Boolean = true,
                             doConfTwitter: Boolean = true,
                             doTeamColor: Boolean = false,
                             doResultInserts: Boolean = true,
                             doResultUpdates: Boolean = true,
                             doResultDeletes: Boolean = false,
                             fromDate: Option[LocalDate] = None,
                             toDate: Option[LocalDate] = None)


