package models.util

import scala.slick.lifted.MappedTypeMapper
import org.joda.time.DateMidnight
import java.sql.Date

object DateMidnightMapper {

  implicit def date2dateTime = MappedTypeMapper.base[DateMidnight, Date](
    dateTime => new Date(dateTime.getMillis),
    date => new DateMidnight(date)
  )

}
