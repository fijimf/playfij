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

    def autoInc =  quote ~ source ~ url returning id

  }

  def list(implicit s:scala.slick.session.Session): List[Quote] = {
    Query(Quotes).sortBy(_.id).to[List]
  }

  def find(id: Long)(implicit s:scala.slick.session.Session): Option[Quote] = {
    Query(Quotes).where(_.id === id).firstOption
  }

  def update(quote: Quote)(implicit s:scala.slick.session.Session) {
    Quotes.where(_.id === quote.id).update(quote)
  }

  def insert(quote: Quote)(implicit s:scala.slick.session.Session) {
    Quotes.autoInc.insert(quote.quote, quote.source, quote.url)
  }

  def delete(id: Long)(implicit s:scala.slick.session.Session) {
    Quotes.where(_.id === id).delete
  }


}
