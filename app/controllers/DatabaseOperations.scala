package controllers

import play.api.mvc.{Action, Controller}
import models.{Repository}

object DatabaseOperations extends Controller {

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
        case e: Exception => Redirect(routes.DatabaseOperations.index()).flashing("error" -> "Problem rebuilding the database")
      }
      Redirect("/app/database").flashing("success" -> "Database rebuilt")
    }
  }

  def scrapeNcaaTeams = Action {
    val data: List[(String, models.Team)] = scraping.PlayTeamScraper.teamRawData()
    play.api.db.slick.DB.withSession {
      try {
        repo.scrapeNcaaTeamsAndConferences(data)
      }
      catch {
        case e: Exception => Redirect(routes.DatabaseOperations.index()).flashing("error" -> "Problem rebuilding the database")
      }
      Redirect("/app/database").flashing("success" -> "Scraped!")
    }
  }
}