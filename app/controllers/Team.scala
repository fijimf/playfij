package controllers

import play.api.mvc.{Controller, Action}
import models.Repository
import play.api.data._
import play.api.data.Forms._

object Team extends Controller {

  import play.api.Play.current

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
          Ok(views.html.teamEdit(oTeam.get))
        } else {
          NotFound("bbbbb")
        }
      }
  }

  def update(key:String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
      val oTeam: Option[models.Team] = repo.getTeam(key)
      if (oTeam.isDefined) {
        Ok(views.html.teamForm(teamForm.fill(oTeam.get),oTeam.get.name+" "+oTeam.get.nickname))
      } else {
        NotFound("bbbbb")
      }
    }
  }

  def create = TODO

  def delete = TODO

  def submit = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        import repo.profile.simple._

        teamForm.bindFromRequest.fold(
          errors => BadRequest(views.html.teamForm(errors, "Save failed")),
          team => {
            repo.u`
            Ok(views.html.teamList(repo.getTeams))
          }
        )
      } //FIXME bad url in address bar

  }


}