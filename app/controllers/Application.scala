package controllers

import play.api.mvc.{Controller, Action}
import models.Repository
import scala.slick.jdbc.meta.MTable
import play.api.Logger
import scala.slick.session.Session

object Application extends Controller {

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

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

  def dataOperations = Action {
    play.api.db.slick.DB.withSession { implicit session:Session=>
      MTable.getTables.foreach((table: MTable) => {
        Logger.info(table.name.toString());
      })
    }
    Ok(views.html.dataOperations("Put in some relevant data stuff"))
  }

  def rebuildDatabase = Action {
    play.api.db.slick.DB.withSession {
      repo.rebuildDatabase
    }
    Ok(views.html.dataOperations("Put in some relevant data stuff"))
  }
}