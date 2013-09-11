package controllers

import play.api.mvc._
import play.api.Logger
import models.Repository
import play.api.data.Form
import play.api.data.Forms._
import scala.Some

object Quote extends Controller {

  import play.api.Play.current

  private val logger = Logger("TeamController")
  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val quoteForm: Form[models.Quote] = Form(
    mapping(
      "id" -> longNumber,
      "quote" -> nonEmptyText,
      "source" -> nonEmptyText,
      "url" -> nonEmptyText
    )(models.Quote.apply)(models.Quote.unapply)
  )

  def list = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        Ok(views.html.quoteList(repo.getQuotes))
      }
  }

  def edit(id: Long) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        loadQuoteAndKeys(id) match {
          case Some((quote, prevId, nextId)) => Ok(views.html.quoteForm(quoteForm.fill(quote), "season.season "Quote"", prevId, nextId))
          case None => NotFound(views.html.resourceNotFound("quote", id))
        }
      }
  }

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {

        quoteForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.quoteForm(errors, "Save failed","#","#"))
          },
          quote => {
            if (quote.id == 0) {
              repo.insertQuote(quote)
              Redirect(routes.Quote.list()).flashing("success" -> ("Added " + quote.quote))
            } else {
              repo.updateQuote(quote)
              Redirect(routes.Quote.list()).flashing("success" -> ("Updated " + quote.quote))
            }
          }
        )
      }
  }

  def create = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val ids: List[String] = repo.quoteIds
        val prevId = ids.last
        val nextId = ids.head

        Ok(views.html.quoteForm(quoteForm.bind(Map.empty[String, String]), "New Quote", prevId, nextId))
      }
  }

  def delete = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val season: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("quote")).flatMap(_.headOption)
        val id: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption)
        id match {
          case Some(x) => {
            repo.deleteQuote(x)
            Redirect(routes.Quote.list()).flashing("success" -> (season.getOrElse("Quote #" + id.get) + " deleted."))
          }
          case None => Redirect(routes.Conference.list()).flashing("error" -> "No id parameter passed to delete")
        }
      }
  }

  def view(key:String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        loadSeasonAndKeys(key) match {
          case Some((quote, prevId, nextId) => Ok(views.html.quoteView(quote, "Quote", previd, nextid))
          case None => NotFound(views.html.resourceNotFound("quote", id))
        }
      }
  }

  def loadQuoteAndKeys(id: Long): Option[(models.Quote, Long, Long)] = {
    val oQuote: Option[models.Quote] = repo.getQuote(key)
    if (oQuote.isDefined) {
      val keys: List[Long] = repo.seasonIds
      val n = keys.indexOf(key)
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
}
