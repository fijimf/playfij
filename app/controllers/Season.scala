package controllers

import play.api.mvc._
import play.api.Logger
import models._
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.db.slick.Profile
import scala.Some
import securesocial.core.SecureSocial

object Season extends Controller with SecureSocial  {

  import play.api.Play.current

  private val model = new Model(){
    val profile = play.api.db.slick.DB.driver
  }

  private val dao: SeasonDao = SeasonDao(model)


  private val logger = Logger("SeasonController")

  val seasonForm: Form[models.Season] = Form(
    mapping(
      "id" -> longNumber,
      "key" -> nonEmptyText,
      "season" -> nonEmptyText,
      "from" -> jodaLocalDate,
      "to" -> jodaLocalDate
    )(models.Season.apply)(models.Season.unapply)
  )

  def list = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

        Ok(views.html.seasonList(dao.list))
      }
  }

  def edit(key: String) = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadSeasonAndKeys(key) match {
          case Some((season, prevKey, nextKey)) => Ok(views.html.seasonForm(seasonForm.fill(season), season.season , prevKey, nextKey))
          case None => NotFound(views.html.resourceNotFound("season", key))
        }
      }
  }

  def submit = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

        seasonForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.seasonForm(errors, "Save failed",Some("#"),Some("#"))) //FIXME
          },
          season => {
            if (season.id == 0) {
              dao.insert(season)
              Redirect(routes.Season.list()).flashing("success" -> ("Added " + season.season))
            } else {
              dao.update(season)
              Redirect(routes.Season.list()).flashing("success" -> ("Updated " + season.season))
            }
          }
        )
      }
  }

  def create = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val keys: List[String] = dao.list.map(_.key)
        val prevKey =  keys.lastOption
        val nextKey =  keys.headOption

        Ok(views.html.seasonForm(seasonForm.bind(Map.empty[String, String]), "New Season", prevKey, nextKey))
      }
  }

  def delete = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val season: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("season")).flatMap(_.headOption)
        val id: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption)
        id match {
          case Some(x) => {
            dao.delete(x)
            Redirect(routes.Season.list()).flashing("success" -> (season.getOrElse("Season #" + id.get) + " deleted."))
          }
          case None => Redirect(routes.Season.list()).flashing("error" -> "No id parameter passed to delete")
        }
      }
  }

  def view(key:String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadSeasonAndKeys(key) match {
          case Some((season, prevKey, nextKey)) => Ok(views.html.seasonView(season, season.season, prevKey, nextKey))
          case None => NotFound(views.html.resourceNotFound("team", key))
        }
      }
  }

  private [this] def loadSeasonAndKeys(key: String)(implicit s:scala.slick.session.Session): Option[(models.Season, Option[String], Option[String])] = {
    val oSeason: Option[models.Season] = dao.find(key)
    if (oSeason.isDefined) {
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
      Some(oSeason.get, Some(prevKey), Some(nextKey))
    } else {
      None
    }
  }


}
