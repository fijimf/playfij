package controllers

import play.api.mvc.{Action, Controller}
import models.{DatabaseStatus, Repository}

object DatabaseOperations extends Controller {

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)


  def index = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        val status: DatabaseStatus = repo.checkDatabase()
        Ok(views.html.dataOperations(status))
      }
  }

  def rebuildDatabase = Action {
    play.api.db.slick.DB.withSession {
      try {
        repo.rebuildDatabase()
      }
      catch {
        case e: Exception => Redirect("/app/database").flashing("error" -> "Problem rebuilding the database")
      }
      Redirect("/app/database").flashing("success" -> "Database rebuilt")
    }
  }

  def scrapeNcaaTeams = Action {
    play.api.db.slick.DB.withSession {

      repo.scrapeNcaaTeamsAndConferences()

      Redirect("/app/database").flashing("success" -> "Database rebuilt")

    }
  }
}