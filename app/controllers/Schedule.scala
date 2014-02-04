package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models._
import models.ScheduleDao
import org.joda.time.LocalDate
import org.saddle.Series
import analysis.ModelRecord
import org.saddle.scalar.Scalar
import org.saddle.stats.RankTie

object Schedule extends Controller with SecureSocial {

  import play.api.Play.current

  private val logger = Logger("ScheduleController")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)
  private val quoteDao = new QuoteDao(model)


  def stat(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>

            Ok(views.html.statView( key))
      }
  }

  def stats() = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val d = scheduleDao.getStatDates (null)
          val ss: List[(Statistic, Series[Team, Double])] = scheduleDao.loadStats(d)
          ss.map{case (stat: Statistic, ser: Series[Team, Double]) => {
            val ix: Int = ser.index.getFirst(team)
            val value: Scalar[Double] = ser.at(ix)
            val rank: Scalar[Double] = ser.rank(RankTie.Max, !stat.higherIsBetter).at(ix)
            val z: Scalar[Double] = value.map(x => (x - ser.mean) / ser.stdev)
            ModelRecord(stat.name, stat.key, cleanString(value, stat.longFormat), cleanString(rank, "%.0f"), cleanString(z, "%4.2f"), stat.displayOrder)
          }
          }

          Ok(views.html.statsView(null))

      }
  }

  def team(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val q = quoteDao.random
          scheduleDao.teamPage(key).map(tp => {
            Ok(views.html.teamView(q, tp))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", key)))
      }
  }

  def teamSeason(key: String, seasonKey: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val q = quoteDao.random
          scheduleDao.teamPage(key, seasonKey).map(tp => {
            Ok(views.html.teamView(q, tp))
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

}

