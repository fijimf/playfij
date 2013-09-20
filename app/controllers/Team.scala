package controllers

import play.api.mvc.{Controller, Action}
import models.{TeamDao, Repository}
import play.api.data._
import play.api.data.Forms._
import play.api.Logger

object Team extends Controller {

  import play.api.Play.current

  private val logger = Logger("TeamController")
  private val dao = TeamDao(play.api.db.slick.DB.driver)

  val teamForm: Form[models.Team] = Form(
    mapping(
      "id" -> longNumber,
      "key" -> nonEmptyText,
      "name" -> nonEmptyText,
      "longName" -> nonEmptyText,
      "nickname" -> nonEmptyText,
      "primaryColor" -> optional(text),
      "secondaryColor" -> optional(text),
      "logoUrl" -> optional(text),
      "officialUrl" -> optional(text),
      "officialTwitter" -> optional(text)
    )(models.Team.apply)(models.Team.unapply)
  )

  def view(key: String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val oTeam: Option[models.Team] = dao.find(key)
        if (oTeam.isDefined) {
          val keys: List[String] = dao.list.map(_.key)
          val n = keys.indexOf(key)
          val prevKey = if (n==0){
             keys.last
          } else {
            keys(n-1)
          }
          val nextKey = if (n==(keys.size-1)){
             keys.head
          } else {
            keys(n+1)
          }
          Ok(views.html.teamView(oTeam.get, oTeam.get.name + " " + oTeam.get.nickname, prevKey, nextKey))
        } else {
          NotFound(views.html.resourceNotFound("team", key))
        }
      }
  }


  def list = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        Ok(views.html.teamList(dao.list))
      }
  }

  def edit(key: String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val oTeam: Option[models.Team] = dao.find(key)
        if (oTeam.isDefined) {
          val keys: List[String] = dao.list.map(_.key)
          val n = keys.indexOf(key)
          val prevKey = if (n==0){
            keys.last
          } else {
            keys(n-1)
          }
          val nextKey = if (n==(keys.size-1)){
            keys.head
          } else {
            keys(n+1)
          }
          Ok(views.html.teamForm(teamForm.fill(oTeam.get), oTeam.get.name + " " + oTeam.get.nickname, prevKey, nextKey))
        } else {
          NotFound(views.html.resourceNotFound("team", key))
        }
      }
  }

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

        teamForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.teamForm(errors, "Save failed","#","#"))
          },
          team => {
            if (team.id == 0) {
              dao.insert(team)
              Redirect(routes.Team.list()).flashing("success" -> ("Added " + team.name))
            } else {
              dao.update(team)
              Redirect(routes.Team.list()).flashing("success" -> ("Updated " + team.name))
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
        val prevKey =  keys.last
        val nextKey =  keys.head

        Ok(views.html.teamForm(teamForm.bind(Map.empty[String, String]), "New Team", prevKey, nextKey))
      }
  }

  def delete = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
        val teamName: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("teamName")).flatMap(_.headOption)
        val id: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption)
        id match {
          case Some(x) => {
            dao.delete(x)
            Redirect(routes.Team.list()).flashing("success" -> (teamName.getOrElse("Team #" + id.get) + " deleted."))
          }
          case None => Redirect(routes.Team.list()).flashing("error" -> "No id parameter passed to delete")
        }
      }
  }

}
