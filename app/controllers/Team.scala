package controllers

import play.api.mvc.{Controller, Action}
import models._
import play.api.data._
import play.api.data.Forms._
import play.api.Logger
import scala.Some
import models.ConferenceAssociationDao
import models.TeamDao
import securesocial.core.SecureSocial
import org.apache.commons.lang3.StringUtils
import org.joda.time.LocalDate

object Team extends Controller with SecureSocial  {

  import play.api.Play.current

  private val logger = Logger("TeamController")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val teamDao: TeamDao = TeamDao(model)
  private val conferenceAssociationDao = new ConferenceAssociationDao(model)

  val keys: List[String] = play.api.db.slick.DB.withSession {
    implicit s => teamDao.list.map(_.key) //TODO snmart cache me!!
  }

  val teamForm: Form[(models.Team, List[String])] = Form(
    mapping(
      "id" -> longNumber,
      "key" -> nonEmptyText,
      "name" -> nonEmptyText,
      "longName" -> nonEmptyText,
      "nickname" -> nonEmptyText,
      "aliases" -> optional(text),
      "primaryColor" -> optional(text),
      "secondaryColor" -> optional(text),
      "logoUrl" -> optional(text),
      "officialUrl" -> optional(text),
      "officialTwitter" -> optional(text)
    )((id, key, name, longName, nickname, aliases, primaryColor, secondaryColor, logoUrl, officialUrl, officialTwitter) =>
      (models.Team(id, key, name, longName, nickname, primaryColor, secondaryColor, logoUrl, officialUrl, officialTwitter), aliases.getOrElse("").split('\n').map(_.trim).toList.filter(s=>StringUtils.isNotBlank(s))))
      (
        (tup: (models.Team, List[String])) =>
          Some(tup._1.id, tup._1.key, tup._1.name, tup._1.longName, tup._1.nickname, Some(tup._2.mkString("\n")), tup._1.primaryColor, tup._1.secondaryColor, tup._1.logoUrl, tup._1.officialUrl, tup._1.officialTwitter)
      )
  )

  val aliasForm: Form[(String, Seq[String])] = Form(
    mapping("alias" -> nonEmptyText,
      "teamKey" -> seq(nonEmptyText))((alias, teamKey) => (alias, teamKey))((tup: (String, Seq[String])) => Some(tup))
  )

  def test = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
            Ok(views.html.teamEg() )
        }

  }


  def list = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          Ok(views.html.teamList(teamDao.listWithAliases))
      }
  }

  def edit(key: String) = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          teamDao.findWithAliases(key).map {
            case (t: Team, as: List[String]) => {
              val (prevKey, nextKey) = nextKeys(key)
              val cmap: Map[Season, Conference] = conferenceAssociationDao.queryByTeam(t)
              Ok(views.html.teamForm(teamForm.fill((t, as)), cmap, t.name + " " + t.nickname, prevKey, nextKey))
            }
          }.getOrElse({
            NotFound(views.html.resourceNotFound("team", key))
          })
      }
  }

  def submit = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

          teamForm.bindFromRequest.fold(
          errors => {
            logger.info("Problems saving " + errors)
            BadRequest(views.html.teamForm(errors, Map.empty[Season, Conference], "Save failed", "#", "#"))
          }, {
            case (team: models.Team, aliases: List[String]) =>
              if (team.id == 0) {
                teamDao.insert(team)
                teamDao.insertAliases(team, aliases)
                Redirect(routes.Team.list()).flashing("success" -> ("Added " + team.name))
              } else {
                teamDao.update(team)
                teamDao.updateAliases(team, aliases)
                Redirect(routes.Team.list()).flashing("success" -> ("Updated " + team.name))
              }
          })
      }
  }

  def create = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val keys: List[String] = teamDao.list.map(_.key)
          val prevKey = keys.last
          val nextKey = keys.head
          Ok(views.html.teamForm(teamForm.bind(Map.empty[String, String]), Map.empty[Season, Conference], "New Team", prevKey, nextKey))
      }
  }

  def delete = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val id: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("id")).flatMap(_.headOption)
          id match {
            case Some(x) => {
              teamDao.delete(x)
              Redirect(routes.Team.list()).flashing("success" -> ("Team:" + x + " deleted."))
            }
            case None => Redirect(routes.Team.list()).flashing("error" -> "No id parameter passed to delete")
          }
      }
  }

  def addAlias = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          aliasForm.bindFromRequest.fold(
          errors => {
            logger.info("Can't happen")
            logger.info(errors.toString)
            Redirect(routes.Team.list())
          }, {
            case (alias: String, teamKey: Seq[String]) =>
              teamDao.addAlias(alias, teamKey(0))
              Redirect(routes.Team.list()).flashing("success" -> ("Added " + alias))
          })
      }
  }

  def nextKeys(key: String): (String, String) = {
    val n = keys.indexOf(key)
    val prevKey = if (n == 0) {
      keys.last
    } else {
      keys(n - 1)
    }
    val nextKey = if (n == (keys.size - 1)) {
      keys.head
    } else {
      keys(n + 1)
    }
    (prevKey, nextKey)
  }

}


