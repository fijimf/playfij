package controllers

import play.api.mvc._
import play.api.Logger
import models._
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.db.slick.Profile
import scala.Some
import models.StatisticalModelDao
import securesocial.core.SecureSocial

object StatisticalModel extends Controller with SecureSocial  {

  import play.api.Play.current
  private val model = new Model(){
    val profile = play.api.db.slick.DB.driver
  }

  private val dao: StatisticalModelDao = StatisticalModelDao(model)

  private val logger = Logger("StatisticalModelController")
  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val statisticalModelForm: Form[models.StatisticalModel] = Form(
    mapping(
      "id" -> longNumber,
      "key" -> nonEmptyText,
      "name" -> text,
      "className" -> text
    )(models.StatisticalModel.apply)(models.StatisticalModel.unapply)
  )

  def list = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          Ok(views.html.statisticalModelList(dao.list))
      }
  }

  def edit(id: Long) = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadStatisticalModel(id) match {
          case Some(statisticalModel) => Ok(views.html.statisticalModelForm(statisticalModelForm.fill(statisticalModel), "StatisticalModel"))
          case None => NotFound(views.html.resourceNotFound("statisticalModel", id.toString))
        }
      }
  }

  def submit = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

        statisticalModelForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.statisticalModelForm(errors, "Save failed"))  //FIXME......
          },
          statisticalModel => {
            if (statisticalModel.id == 0) {
              dao.insert(statisticalModel)
              Redirect(routes.StatisticalModel.list()).flashing("success" -> ("Added " + statisticalModel.key))
            } else {
              dao.update(statisticalModel)
              Redirect(routes.StatisticalModel.list()).flashing("success" -> ("Updated " + statisticalModel.key))
            }
          }
        )
      }
  }

  def create = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        Ok(views.html.statisticalModelForm(statisticalModelForm.bind(Map.empty[String, String]), "New StatisticalModel"))
      }
  }

  def delete = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val season: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("statisticalModel")).flatMap(_.headOption)
        val id: Option[Long] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption).map(_.toLong)
        id match {
          case Some(x) => {
            dao.delete(x)
            Redirect(routes.StatisticalModel.list()).flashing("success" -> (season.getOrElse("StatisticalModel #" + id.get) + " deleted."))
          }
          case None => Redirect(routes.Conference.list()).flashing("error" -> "No id parameter passed to delete")
        }
      }
  }

  def view(id:Long) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        loadStatisticalModel(id) match {
          case Some(statisticalModel) => Ok(views.html.statisticalModelView(statisticalModel, "StatisticalModel"))
          case None => NotFound(views.html.resourceNotFound("statisticalModel", id.toString))
        }
      }
  }

  private [this] def loadStatisticalModel(id: Long)(implicit s:scala.slick.session.Session): Option[models.StatisticalModel] = {
    dao.find(id)
  }
}
