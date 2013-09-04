package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile

case class Quote(id: Long,
                 quote: String,
                 source: Option[String],
                 url: Option[String]) {
  require(StringUtils.isNotBlank(quote))
}

trait QuoteDao {

  self: Profile =>

  import profile.simple._

  object Quotes extends Table[Quote]("quotes") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def quote = column[String]("quote")

    def source = column[Option[String]]("source")

    def url = column[Option[String]]("url")

    def * = id ~ quote ~ source ~ url <>(Quote.apply _, Quote.unapply _)

    def autoInc = id ~ quote ~ source ~ url <>(Quote.apply _, Quote.unapply _)

  }

}
