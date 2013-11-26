package controllers.admin

import play.api.mvc.Controller
import models._
import play.api.Logger
import securesocial.core.SecureSocial
import analysis.ComputableModel
import org.joda.time.LocalDate
import scala.slick.lifted.Query


object RunModels extends Controller with SecureSocial {
  val logger = Logger("RunModels")

  import play.api.Play.current

  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  def saveOrUpdate(statModel: StatisticalModel, result: Map[String, Map[LocalDate, Map[Long, Double]]])(implicit s: scala.slick.session.Session) {

  }

  def run = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val statModelDao: StatisticalModelDao = StatisticalModelDao(model)
          val statDao: StatisticDao = StatisticDao(model)
          val obsDao: ObservationDao = ObservationDao(model)
          val models: List[StatisticalModel] = statModelDao.list
          val scheduleData: List[ScheduleData] = ScheduleDao(model).loadScheduleData

          models.foreach(statModel => {
            val computableModel: ComputableModel = Class.forName(statModel.className).newInstance().asInstanceOf[ComputableModel]
            val result = computableModel.compute(scheduleData)


          })
          Redirect(controllers.admin.routes.Admin.index)
      }
  }

}

