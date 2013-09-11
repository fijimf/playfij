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
      "key" -> nonEmptyText,
      "season" -> nonEmptyText,
      "from" -> jodaLocalDate,
      "to" -> jodaLocalDate
    )(models.Season.apply)(models.Season.unapply)
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
        loadSeasonAndKeys(key) match {
          case Some((season, prevKey, nextKey)) => Ok(views.html.seasonForm(seasonForm.fill(season), season.season , prevKey, nextKey))
          case None => NotFound(views.html.resourceNotFound("season", key))
        }
      }
  }

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {

        seasonForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.seasonForm(errors, "Save failed","#","#"))
          },
          season => {
            if (season.id == 0) {
              repo.insertSeason(season)
              Redirect(routes.Season.list()).flashing("success" -> ("Added " + season.season))
            } else {
              repo.updateSeason(season)
              Redirect(routes.Season.list()).flashing("success" -> ("Updated " + season.season))
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

        Ok(views.html.seasonForm(seasonForm.bind(Map.empty[String, String]), "New Season", prevKey, nextKey))
      }
  }

  def delete = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val season: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("season")).flatMap(_.headOption)
        val id: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption)
        id match {
          case Some(x) => {
            repo.deleteSeason(x)
            Redirect(routes.Season.list()).flashing("success" -> (season.getOrElse("Season #" + id.get) + " deleted."))
          }
          case None => Redirect(routes.Conference.list()).flashing("error" -> "No id parameter passed to delete")
        }
      }
  }

  def view(key:String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        loadSeasonAndKeys(key) match {
          case Some((season, prevKey, nextKey)) => Ok(views.html.seasonView(season, season.season, prevKey, nextKey))
          case None => NotFound(views.html.resourceNotFound("team", key))
        }
      }
  }

  def loadSeasonAndKeys(key: String): Option[(models.Season, String, String)] = {
    val oSeason: Option[models.Season] = repo.getSeason(key)
    if (oSeason.isDefined) {
      val keys: List[String] = repo.seasonKeys
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
      Some(oSeason.get, prevKey, nextKey)
    } else {
      None
    }
  }


}
