
import _root_.controllers.admin.RunModels
import java.util.concurrent.TimeUnit
import models.Repository
import org.joda.time.{LocalDate, DateTime}
import play.api._
import play.api.libs.concurrent.Akka
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.slick.session.Session
import scraping.control.GameUpdateRequest
import scraping.NcaaGameScraper

object Global extends GlobalSettings {
  val logger = Logger(Global.getClass)

  import play.api.Play.current
  import ExecutionContext.Implicits.global

  override def onStart(app: Application) {
    super.onStart(app)
    schedule()
  }

  def schedule() {
    try {
      val now: DateTime = new DateTime()
      val clock = now.withHourOfDay(0).withMinuteOfHour(45).withSecondOfMinute(0).withMillisOfSecond(0)
      val d = if (clock.isBefore(now)) {
        logger.info("Scheduling job to run at " + (clock.plusDays(1)))
        Duration.create(clock.plusDays(1).getMillis - now.getMillis, TimeUnit.MILLISECONDS)
      } else {
        logger.info("Scheduling job to run at " + clock)
        Duration.create(clock.getMillis - now.getMillis, TimeUnit.MILLISECONDS)
      }

      Akka.system.scheduler.scheduleOnce(d) {
        val from: LocalDate = new LocalDate().minusDays(2)
        val to: LocalDate = new LocalDate().plusDays(7)
        logger.info("Running scheduled job updating games and results")
        val req = GameUpdateRequest("", true, true, true, true, true, true, true, Some(from), Some(to))
        val repo: Repository = new Repository(play.api.db.slick.DB.driver)
        play.api.db.slick.DB.withSession { implicit s:Session=>
          val result = NcaaGameScraper.scrape(repo, req)
          logger.info("Results inserted: " + result.resultsInserted.size);
          RunModels.runModelsForDates(from, to)
        }
        schedule(); //Schedule for next time
      }
    } catch {
      case (e: Exception) => logger.error("", e)
    }
  }


}
