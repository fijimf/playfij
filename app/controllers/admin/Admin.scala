package controllers.admin

import play.api.mvc.{Controller, Action}
import models.Repository

object Admin extends Controller {
  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  def index = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val status: List[(String, Option[Int])] = repo.checkDatabase()
        Ok(views.html.adminIndex(status))
      }
  }

}
