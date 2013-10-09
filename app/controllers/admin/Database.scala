package controllers.admin

import models.Repository
import play.api.mvc.{Action, Controller}

object Database extends Controller {

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  def index = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val status: List[(String, Option[Int])] = repo.checkDatabase()
        Ok(views.html.dataOperations(status))
      }
  }

  def rebuildDatabase = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        try {
          repo.rebuildDatabase()
        }
        catch {
          case e: Exception => Redirect(routes.Database.index()).flashing("error" -> "Problem rebuilding the database")
        }
        Redirect(routes.Database.index()).flashing("success" -> "Database rebuilt")
      }
  }
}
