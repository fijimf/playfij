package controllers

import play.api.mvc._
import models.SeasonDao
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile
import play.api.Logger

class Repo(override val profile: ExtendedProfile) extends SeasonDao with Profile {
  def xxx() {
    Seasons.ddl.createStatements.foreach((s: String) => {
      Logger.info(s)
    })
  }
}


object Application extends Controller {

  import play.api.Play.current

  def index = Action {
    val repo: Repo = new Repo(play.api.db.slick.DB.driver)
    repo.xxx()
    Ok(views.html.index("Your new application is ready."))
  }


  def gameList = Action {
    Ok(views.html.gameList("Your new application is ready."))
  }

  def conferenceList = Action {
    Ok(views.html.notImplemented("Your new application is ready."))
  }

  def teamList = Action {

    Ok(views.html.notImplemented("Teamlist not implemented."))
  }
}