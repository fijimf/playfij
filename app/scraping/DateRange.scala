package scraping

import org.joda.time.LocalDate

object DateRange {
  def apply(from: LocalDate, to: LocalDate):DateRange = DateRange(Some(from), Some(to))
}

case class DateRange(from: Option[LocalDate], to: Option[LocalDate]) {
  def contains(d: LocalDate) = {
    from.map(f => f.isEqual(d) || f.isBefore(d)).getOrElse(true) &&
      to.map(t => t.isEqual(d) || t.isAfter(d)).getOrElse(true)
  }


}
