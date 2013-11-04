package controllers

import play.api.mvc.{Controller, Action}
import models.{DatabaseStatus, Repository}
import securesocial.core.SecureSocial

object Application extends Controller with SecureSocial  {


  def index = UserAwareAction {
    implicit request =>

    Ok(views.html.index("Your new application is ready."))
  }

}