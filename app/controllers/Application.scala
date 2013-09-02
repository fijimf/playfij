package controllers

import play.api.mvc.{Controller, Action}
import models.{DatabaseStatus, Repository}

object Application extends Controller {


  def index = Action {
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