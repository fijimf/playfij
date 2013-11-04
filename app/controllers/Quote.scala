package controllers

import play.api.mvc._
import play.api.Logger
import models._
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.db.slick.Profile
import scala.Some
import models.QuoteDao
import securesocial.core.SecureSocial

object Quote extends Controller with SecureSocial  {

  import play.api.Play.current
  private val model = new Model(){
    val profile = play.api.db.slick.DB.driver
  }

  private val dao: QuoteDao = QuoteDao(model)

  private val logger = Logger("QuoteController")
  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val quoteForm: Form[models.Quote] = Form(
    mapping(
      "id" -> longNumber,
      "quote" -> nonEmptyText,
      "source" -> optional(text),
      "url" -> optional(text)
    )(models.Quote.apply)(models.Quote.unapply)
  )

  def list = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          Ok(views.html.quoteList(dao.list))
      }
  }

  def edit(id: Long) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadQuoteAndKeys(id) match {
          case Some((quote, prevId, nextId)) => Ok(views.html.quoteForm(quoteForm.fill(quote), "Quote", prevId, nextId))
          case None => NotFound(views.html.resourceNotFound("quote", id.toString))
        }
      }
  }

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

        quoteForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.quoteForm(errors, "Save failed",0,0))  //FIXME......
          },
          quote => {
            if (quote.id == 0) {
              dao.insert(quote)
              Redirect(routes.Quote.list()).flashing("success" -> ("Added " + quote.quote))
            } else {
              dao.update(quote)
              Redirect(routes.Quote.list()).flashing("success" -> ("Updated " + quote.quote))
            }
          }
        )
      }
  }

  def create = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val ids: List[Long] = dao.list.map(_.id)
        val prevId = ids.last
        val nextId = ids.head

        Ok(views.html.quoteForm(quoteForm.bind(Map.empty[String, String]), "New Quote", prevId, nextId))
      }
  }

  def delete = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val season: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("quote")).flatMap(_.headOption)
        val id: Option[Long] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption).map(_.toLong)
        id match {
          case Some(x) => {
            dao.delete(x)
            Redirect(routes.Quote.list()).flashing("success" -> (season.getOrElse("Quote #" + id.get) + " deleted."))
          }
          case None => Redirect(routes.Conference.list()).flashing("error" -> "No id parameter passed to delete")
        }
      }
  }

  def view(id:Long) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadQuoteAndKeys(id) match {
          case Some((quote, prevId, nextId)) => Ok(views.html.quoteView(quote, "Quote", prevId, nextId))
          case None => NotFound(views.html.resourceNotFound("quote", id.toString))
        }
      }
  }

  def loadQuoteAndKeys(id: Long)(implicit s:scala.slick.session.Session): Option[(models.Quote, Long, Long)] = {
    val oQuote: Option[models.Quote] = dao.find(id)
    if (oQuote.isDefined) {
      val keys: List[Long] = dao.list.map(_.id)
      val n = keys.indexOf(id)
      val prevKey = if (n == 0) {
        keys.last
      } else {
        keys(n - 1)
      }
      val nextKey = if (n == (keys.size - 1)) {
        keys.head
      } else {
        keys(n + 1)
      }
      Some(oQuote.get, prevKey, nextKey)
    } else {
      None
    }
  }
}
