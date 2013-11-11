package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models.{ScheduleDao, ConferenceAssociationDao, TeamDao, Model}

object Schedule extends Controller with SecureSocial {
  import play.api.Play.current

  private val logger = Logger("ScheduleController")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)


  def team(key:String)  = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          scheduleDao.teamPage(key).map(tp => {
            Ok(views.html.teamView(tp))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", key)))
      }
  }


}

