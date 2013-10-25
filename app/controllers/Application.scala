package controllers

import play.api.mvc.{Controller, Action}
import models.{DatabaseStatus, Repository}

object Application extends Controller {


  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def team(key: String) = TODO

  def conference(key: String) = TODO

  def statistic(key: String) = TODO

  def histStatistic(key: String, date: String) = TODO

}