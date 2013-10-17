package controllers.admin

import play.api.mvc.{Controller, Action}
import models.Repository

object Ncaa extends Controller {

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  def index = Action {
    Ok(views.html.ncaaIndex(""))
  }

  def scrapeTeams = Action {
    val data: List[(String, models.Team)] = scraping.PlayTeamScraper.teamRawData()
    play.api.db.slick.DB.withSession {
      try {
        repo.scrapeNcaaTeamsAndConferences(data)
      }
      catch {
        case e: Exception => Redirect(routes.Database.index()).flashing("error" -> "Problem rebuilding the database")
      }
      Redirect("/app/database").flashing("success" -> "Scraped!")
    }
  }

  def scrapeGames = TODO
  def scrapeGamesEtc = TODO


}
