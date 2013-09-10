package controllers

import play.api.mvc._
import play.api.Logger
import models.Repository
import play.api.data.Form
import play.api.data.Forms._
import scala.Some

object Season extends Controller {

  import play.api.Play.current

  private val logger = Logger("TeamController")
  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val seasonForm: Form[models.Season] = Form(
    mapping(
      "id" -> longNumber,
      "season" -> nonEmptyText,
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
        Ok(views.html.conferenceList(repo.getConferences))
      }
  }

  def edit(key: String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        loadConferenceAndKeys(key) match {
          case Some((conf, prevKey, nextKey)) => Ok(views.html.conferenceForm(conferenceForm.fill(conf), conf.name , prevKey, nextKey))
          case None => NotFound(views.html.resourceNotFound("team", key))
        }
      }
  }

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {

        conferenceForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.conferenceForm(errors, "Save failed","#","#"))
          },
          conference => {
            if (conference.id == 0) {
              repo.insertConference(conference)
              Redirect(routes.Conference.list()).flashing("success" -> ("Added " + conference.name))
            } else {
              repo.updateConference(conference)
              Redirect(routes.Conference.list()).flashing("success" -> ("Updated " + conference.name))
            }
          }
        )
      }
  }

  def create = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val keys: List[String] = repo.conferenceKeys
        val prevKey =  keys.last
        val nextKey =  keys.head

        Ok(views.html.conferenceForm(conferenceForm.bind(Map.empty[String, String]), "New Conference", prevKey, nextKey))
      }
  }

  def delete = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val confName: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("conferenceName")).flatMap(_.headOption)
        val id: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption)
        id match {
          case Some(x) => {
            repo.deleteConference(x)
            Redirect(routes.Conference.list()).flashing("success" -> (confName.getOrElse("Conference #" + id.get) + " deleted."))
          }
          case None => Redirect(routes.Conference.list()).flashing("error" -> "No id parameter passed to delete")
        }
      }
  }

  def view(key:String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        loadConferenceAndKeys(key) match {
          case Some((conf, prevKey, nextKey)) => Ok(views.html.conferenceView(conf, conf.name , prevKey, nextKey))
          case None => NotFound(views.html.resourceNotFound("team", key))
        }
      }
  }

  def loadConferenceAndKeys(key: String): Option[(models.Conference, String, String)] = {
    val oConf: Option[models.Conference] = repo.getConference(key)
    if (oConf.isDefined) {
      val keys: List[String] = repo.conferenceKeys
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
