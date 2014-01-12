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
import play.api.libs.json.Json._
object Quote extends Controller with SecureSocial  {

  import play.api.Play.current
  private val model = new Model(){
    val profile = play.api.db.slick.DB.driver
  }

  private val dao: QuoteDao = QuoteDao(model)

  private val logger = Logger("QuoteController")

  val quoteForm: Form[models.Quote] = Form(
    mapping(
      "id" -> longNumber,
      "quote" -> nonEmptyText,
      "source" -> optional(text),
      "url" -> optional(text)
    )(models.Quote.apply)(models.Quote.unapply)
  )

  def list = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          Ok(views.html.quoteList(dao.list))
      }
  }

  def edit(id: Long) = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadQuote(id) match {
          case Some(quote) => Ok(views.html.quoteForm(quoteForm.fill(quote), "Quote"))
          case None => NotFound(views.html.resourceNotFound("quote", id.toString))
        }
      }
  }

  def submit = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

        quoteForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.quoteForm(errors, "Save failed"))  //FIXME......
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

  def create = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          Ok(views.html.quoteForm(quoteForm.bind(Map.empty[String, String]), "New Quote"))
      }
  }

  def random = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          dao.random.map(q=>{
            Ok(toJson(
              Map("quote" -> q.quote, "source"->q.source.getOrElse(""), "url"->q.url.getOrElse(""))
            ))
          }).getOrElse(
              Ok(toJson(
                Map("quote" -> "Oh crap -- something is wrong", "source"->"Server", "url"->"http://www.google.com")
              ))
            )
      }
  }

  def delete = SecuredAction {
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

  def view(id:Long) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadQuote(id) match {
          case Some(quote) => Ok(views.html.quoteView(quote, "Quote"))
          case None => NotFound(views.html.resourceNotFound("quote", id.toString))
        }
      }
  }

  private [this] def loadQuote(id: Long)(implicit s:scala.slick.session.Session): Option[models.Quote] = {
    dao.find(id)
  }
}
