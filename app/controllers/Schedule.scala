package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models._
import models.ScheduleDao
import org.joda.time.{DateTime, LocalDate}
import org.saddle.Series
import analysis.ModelRecord
import org.saddle.scalar.Scalar
import org.saddle.stats.RankTie
import scala.collection.immutable.IndexedSeq

object Schedule extends Controller with SecureSocial {

  import play.api.Play.current

  private val logger = Logger("ScheduleController")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)
  private val teamDao = TeamScheduleDao(model)
  private val quoteDao = new QuoteDao(model)


  def stat(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

          Ok(views.html.statView(key))
      }
  }

  def stats() = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val d = scheduleDao.getStatDates(null)
          val ss: List[(Statistic, Series[Team, Double])] = scheduleDao.loadStats(d)
          val data: List[(Statistic, List[(Team,ModelRecord)])] = ss.map {
            case (stat: Statistic, ser: Series[Team, Double]) => {
              val rankings: List[(Team, ModelRecord)] = 0.until(ser.length).map(i => (ser.index.at(i).get, ModelRecord.fromStatValue(stat, i, ser))).filter(p => p._2.rank.toInt <= 15).sortBy(p => p._2.rank.toInt).take(20).toList
              (stat, rankings)
            }
          }.sortBy(_._1.displayOrder)
          Ok(views.html.statsView(data))
      }
  }

  def team(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          teamDao.teamSummary(key).map(tp => {
            Ok(views.html.teamView( tp))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", key)))
      }
  }

  def teamSeason(key: String, seasonKey: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          teamDao.teamSummary(key, seasonKey).map(tp => {
            Ok(views.html.teamView( tp))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", key)))
      }
  }

  def teams() = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          scheduleDao.teamsPage.map(tp => {
            Ok(views.html.teamsView(tp))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", "any of the blessed suckers")))
      }
  }

  def search(q: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val teams: List[(Team, Conference)] = scheduleDao.search(q).sortBy(_._1.name)
          val quote = quoteDao.random
          if (teams.size == 1) {
            Redirect(routes.Schedule.team(teams.head._1.key))
          } else {
            Ok(views.html.searchView(quote, teams))
          }
      }
  }

  def date(yyyy: Int, mm: Int, dd: Int) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val d = new LocalDate(yyyy, mm, dd)
          val q = quoteDao.random
          Ok(views.html.dateView(q, scheduleDao.datePage(d)))
      }
  }
  def index = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
      val today: LocalDate = new LocalDate()
      val q = quoteDao.random
      Ok(views.html.dateView(q, scheduleDao.datePage(today)))
  }
  }
}

