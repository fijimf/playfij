package controllers.admin

import play.api.mvc.{Controller, Action}
import models.Repository
import securesocial.core.SecureSocial

object Admin extends Controller with SecureSocial {
  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  def index = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val status: List[(String, Option[Int])] = repo.checkDatabase()
        Ok(views.html.adminIndex(status))
      }
  }

}
