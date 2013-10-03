package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile

case class Quote(id: Long,
                 quote: String,
                 source: Option[String],
                 url: Option[String]) {
  require(StringUtils.isNotBlank(quote))
}

case class QuoteDao(model: Model) {

  import model._
  import model.profile.simple._

  def list(implicit s: scala.slick.session.Session): List[Quote] = {
    Query(Quotes).sortBy(_.id).to[List]
  }

  def find(id: Long)(implicit s: scala.slick.session.Session): Option[Quote] = {
    Query(Quotes).where(_.id === id).firstOption
  }

  def update(quote: Quote)(implicit s: scala.slick.session.Session) {
    Quotes.where(_.id === quote.id).update(quote)
  }

  def insert(quote: Quote)(implicit s: scala.slick.session.Session) {
    Quotes.autoInc.insert(quote.quote, quote.source, quote.url)
  }

  def delete(id: Long)(implicit s: scala.slick.session.Session) {
    Quotes.where(_.id === id).delete
  }


}
