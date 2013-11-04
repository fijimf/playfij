package controllers

import play.api.mvc._
import play.api.Logger
import models.{TeamDao, Model, ConferenceDao}
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.db.slick.Profile
import securesocial.core.SecureSocial

object Conference extends Controller with SecureSocial  {

  import play.api.Play.current

  private val model = new Model(){
    val profile = play.api.db.slick.DB.driver
  }

  private val dao: ConferenceDao = ConferenceDao(model)

  private val logger = Logger("TeamController")

  val conferenceForm: Form[models.Conference] = Form(
    mapping(
      "id" -> longNumber,
      "key" -> nonEmptyText,
      "name" -> nonEmptyText,
      "shortName" -> nonEmptyText,
      "logoUrl" -> optional(text),
      "officialUrl" -> optional(text),
      "officialTwitter" -> optional(text)
    )(models.Conference.apply)(models.Conference.unapply)
  )

  def list = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          Ok(views.html.conferenceList(dao.list))
      }
  }

  def edit(key: String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          loadConferenceAndKeys(key) match {
            case Some((conf, prevKey, nextKey)) => Ok(views.html.conferenceForm(conferenceForm.fill(conf), conf.name, prevKey, nextKey))
            case None => NotFound(views.html.resourceNotFound("team", key))
          }
      }
  }

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

          conferenceForm.bindFromRequest.fold(
            errors => {
              logger.info("Problems saving " + errors)
              BadRequest(views.html.conferenceForm(errors, "Save failed", "#", "#"))
            },
            conference => {
              if (conference.id == 0) {
                dao.insert(conference)
                Redirect(routes.Conference.list()).flashing("success" -> ("Added " + conference.name))
              } else {
                dao.update(conference)
                Redirect(routes.Conference.list()).flashing("success" -> ("Updated " + conference.name))
              }
            }
          )
      }
  }

  def create = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val keys: List[String] = dao.list.map(_.key)
          val prevKey = keys.last
          val nextKey = keys.head

          Ok(views.html.conferenceForm(conferenceForm.bind(Map.empty[String, String]), "New Conference", prevKey, nextKey))
      }
  }

  def delete = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val confName: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("conferenceName")).flatMap(_.headOption)
          val id: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption)
          id match {
            case Some(x) => {
              dao.delete(x)
              Redirect(routes.Conference.list()).flashing("success" -> (confName.getOrElse("Conference #" + id.get) + " deleted."))
            }
            case None => Redirect(routes.Conference.list()).flashing("error" -> "No id parameter passed to delete")
          }
      }
  }

  def view(key: String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          loadConferenceAndKeys(key) match {
            case Some((conf, prevKey, nextKey)) => Ok(views.html.conferenceView(conf, conf.name, prevKey, nextKey))
            case None => NotFound(views.html.resourceNotFound("team", key))
          }
      }
  }

  def loadConferenceAndKeys(key: String)(implicit s: scala.slick.session.Session): Option[(models.Conference, String, String)] = {
    val oConf: Option[models.Conference] = dao.find(key)
    if (oConf.isDefined) {
      val keys: List[String] = dao.list.map(_.key)
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
      Some(oConf.get, prevKey, nextKey)
    } else {
      None
    }
  }


}
