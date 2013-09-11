package models.util

import scala.slick.lifted.MappedTypeMapper
import org.joda.time.{LocalDate, DateMidnight}
import java.sql.Date

object LocalDateMapper {

  implicit def date2dateTime = MappedTypeMapper.base[LocalDate, Date](
    dateTime => new Date(dateTime.toDate.getTime),
    date => new LocalDate(date)
  )

}
