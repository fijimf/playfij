package controllers

import play.api.mvc.{Controller, Action}
import models.{Repository}

object Team extends Controller {

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  def list = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        Ok(views.html.teamList(repo.getTeams))
      }
  }
  def edit(key:String) = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val oTeam: Option[models.Team] = repo.getTeam(key)
        if (oTeam.isDefined){
          Ok(views.html.teamEdit(oTeam.get))
        } else {
          NotFound("bbbbb")
        }
      }
  }



}