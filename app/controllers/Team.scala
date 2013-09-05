package controllers

import play.api.mvc.{Controller, Action}
import models.Repository
import play.api.data._
import play.api.data.Forms._
import play.api.Logger

object Team extends Controller {

  import play.api.Play.current

  private val logger = Logger("TeamController")
  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

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


  def list = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        Ok(views.html.teamList(repo.getTeams))
      }
  }

  def edit(key: String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val oTeam: Option[models.Team] = repo.getTeam(key)
        if (oTeam.isDefined) {
          Ok(views.html.teamForm(teamForm.fill(oTeam.get), oTeam.get.name + " " + oTeam.get.nickname))
        } else {
          NotFound("bbbbb")
        }
      }
  }

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {

        teamForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.teamForm(errors, "Save failed"))
          },
          team => {
            repo.updateTeam(team)
            Redirect(routes.Team.list())
          }
        )
      }
  }

  def create = TODO

  def delete = TODO


}