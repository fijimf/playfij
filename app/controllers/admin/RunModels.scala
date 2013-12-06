package controllers.admin

import play.api.mvc.Controller
import models._
import play.api.Logger
import securesocial.core.SecureSocial
import analysis.{ComputableModel}
import org.joda.time.LocalDate
import scala.slick.lifted.Query
import play.api.data.Form
import scraping.control.GameUpdateRequest
import play.api.data.Forms._
import models.ScheduleData
import models.StatisticDao
import models.Statistic
import scraping.control.GameUpdateRequest
import models.Observation
import scala.Some
import models.StatisticalModelDao
import models.StatisticalModel
import models.ObservationDao
import models.ScheduleDao
import scraping.NcaaGameScraper


object RunModels extends Controller with SecureSocial {
  val logger = Logger("RunModels")
  import play.api.Play.current

  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  val runForm: Form[(LocalDate, LocalDate)] = Form(tuple("from" -> jodaLocalDate, "to" -> jodaLocalDate))


  def index = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val statModelDao: StatisticalModelDao = StatisticalModelDao(model)
          val models: List[String] = statModelDao.list.map(_.name)
          Ok(views.html.runModelIndex(runForm))
      }
  }

  def run = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          runForm.bindFromRequest.fold(
            errors => {
              logger.info("Problems saving " + errors)
              BadRequest(views.html.runModelIndex(errors))
            },
            req => {
              val (from, to)=req
              val statModelDao: StatisticalModelDao = StatisticalModelDao(model)
              val statDao: StatisticDao = StatisticDao(model)
              val obsDao: ObservationDao = ObservationDao(model)
              val models: List[StatisticalModel] = statModelDao.list
              val scheduleData: List[ScheduleData] = ScheduleDao(model).loadScheduleData

              models.foreach(statModel => {
                val computableModel: ComputableModel = Class.forName(statModel.className).newInstance().asInstanceOf[ComputableModel]
                val result = computableModel.compute(scheduleData)
                for (sk <- result.keys) {
                  val stat: Statistic = statDao.findByKey(sk) match {
                    case Some(statistic) => statistic
                    case None => {
                      computableModel.statistics.get(sk).map(_.copy(modelId = statModel.id)).foreach(st => statDao.insert(st))
                      statDao.findByKey(sk).get
                    }
                  }
                  val inner: Map[LocalDate, Map[Long, Double]] = result(sk)
                  for (d <- inner.keys if (!d.isBefore(from) && !d.isAfter(to))) {
                    obsDao.deleteByDateStat(stat.id, d)
                    for (id <- inner(d).keys) {
                      obsDao.insert(Observation(-1, d, id, stat.id, inner(d)(id)))
                    }
                  }
                }
              })
              Redirect(controllers.admin.routes.Admin.index)
            }
          )
      }
  }
}

