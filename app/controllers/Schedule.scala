package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models.{ConferenceAssociationDao, TeamDao, Model}

object Schedule extends Controller with SecureSocial {
  import play.api.Play.current

  private val logger = Logger("ScheduleController")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val teamDao: TeamDao = TeamDao(model)
  private val conferenceAssociationDao = new ConferenceAssociationDao(model)


  def team(key:String)  = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          teamDao.find(key).map(t => {
            conferenceAssociationDao.queryByTeam(t)
            Ok(views.html.teamView(t))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", key)))
      }
  }


}

