
import _root_.controllers.admin.RunModels
import com.typesafe.plugin._
import controllers.Schedule
import java.net.InetAddress
import java.util.concurrent.TimeUnit
import models.DatePage
import models.DatePage
import models.ScheduleDao
import models.ScheduleDao
import models.{DatePage, Model, ScheduleDao, Repository}
import org.joda.time.{LocalDateTime, LocalDate, DateTime}
import play.api._
import play.api.libs.concurrent.Akka
import play.api.libs.ws.WS
import play.api.mvc.WithFilters
import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.slick.session.Session
import scala.Some
import scala.Some
import scala.util.control.NonFatal
import scraping.control.GameUpdateRequest
import scraping.control.GameUpdateRequest
import scraping.control.GameUpdateRequest
import scraping.NcaaGameScraper

import com.kenshoo.play.metrics.MetricsFilter
import play.api.mvc._


object Global extends WithFilters(MetricsFilter) {
  val logger = Logger(Global.getClass)

  import play.api.Play.current
  import ExecutionContext.Implicits.global

  override def onStart(app: Application) {
    super.onStart(app)
    schedule(new LocalDateTime())
    warmCache()
    notifyRestart()
  }

  def warmCache() {
    Akka.system.scheduler.schedule(Duration.create(15, TimeUnit.SECONDS), Duration.create(1, TimeUnit.HOURS)) {
      val model = new Model() {
        val profile = play.api.db.slick.DB.driver
      }
      val scheduleDao = ScheduleDao(model)
      val today = new LocalDate()
      play.api.db.slick.DB.withSession {
        implicit s: Session =>
          val data: Map[LocalDate, Option[DatePage]] = (-1).to(1).map(i => today.plusDays(i)).map(d => {
            try {
              logger.info("BEGIN Warming cache for date %s".format(d.toString("yyyy-MM-dd")))
              val p: DatePage = scheduleDao.datePage(d)
              logger.info("END Warming cache for date %s".format(d.toString("yyyy-MM-dd")))
              d -> Some(p)
            }
            catch {
              case ex: Exception =>
                logger.error("Caught exception warming the cache " + ex.getMessage)
                d -> None
            }
          }).toMap

          try {
            data(today).foreach{dp=>
            val mail = use[MailerPlugin].email
            mail.setSubject("Morning Line - %s".format(today.toString("yyyy-MM-dd")))
            mail.setRecipient("fijimf@gmail.com")
            mail.setFrom("Deep Fij <deepfij@gmail.com>")
            mail.sendHtml(views.html.morningLine(dp).body)
            }
          }
          catch {
            case NonFatal(e) => logger.error("CaughtException trying to send mail.", e)
          }
      }
    }
  }

  def schedule(now: LocalDateTime) {
    try {

      val dateTime: DateTime = now.toDateTime
      val clock = dateTime.withHourOfDay(10).withMinuteOfHour(45).withSecondOfMinute(0).withMillisOfSecond(0)
      val d = if (clock.isBefore(dateTime)) {
        logger.info("Scheduling job to run at " + clock.plusDays(1))
        Duration.create(clock.plusDays(1).getMillis - dateTime.getMillis, TimeUnit.MILLISECONDS)
      } else {
        logger.info("Scheduling job to run at " + clock.toString("yyyy-MM-dd HH:mm:ss.SS"))
        Duration.create(clock.getMillis - dateTime.getMillis, TimeUnit.MILLISECONDS)
      }

      Akka.system.scheduler.scheduleOnce(d) {
        val now = new LocalDateTime()
        if (now.getHourOfDay == 10) {
          val from: LocalDate = now.toLocalDate.minusDays(2)
          val to: LocalDate = now.toLocalDate.plusDays(7)
          logger.info("Running scheduled job updating games and results")
          val req = GameUpdateRequest("", doWrite = true, doGameInserts = true, doGameUpdates = true, doGameDeletes = true, doResultInserts = true, doResultUpdates = true, doResultDeletes = true, Some(from), Some(to))
          val repo: Repository = new Repository(play.api.db.slick.DB.driver)
          play.api.db.slick.DB.withSession {
            implicit s: Session =>
              val result = NcaaGameScraper.scrape(repo, req)
              logger.info("Results inserted: " + result.resultsInserted.size)
              RunModels.runModelsForDates(from, to)
              emailAdmin("Schedule Updated") {
                ("Schedule updated:\n\n" +
                  "Games Inserted:\n%s\nGames Updated:\n%s\nGames Deleted:\n%s\n" +
                  "Results Inserted:\n%s\nResults Updated:\n%s\nResults Deleted:\n%s\n").format(
                    result.gamesInserted.map(_.toString).mkString("\n"),
                    result.gamesUpdated.map(_.toString).mkString("\n"),
                    result.gamesDeleted.map(_.toString).mkString("\n"),
                    result.resultsInserted.map(_.toString).mkString("\n"),
                    result.resultsUpdated.map(_.toString).mkString("\n"),
                    result.resultsDeleted.map(_.toString).mkString("\n")
                  )
              }
          }
        } else {
          logger.info("Skipping schedule update because the time is not right -- probably a graceful shutdown.")
        }

        try {
          schedule(now)
        }
        catch {
          case e: IllegalStateException => logger.info("Caught IllegalStateException on scheduling -- probably a graceful shutdown")
        }
      }
    } catch {
      case (e: Exception) => logger.error("", e)
    }
  }

  def notifyRestart() {
    emailAdmin("Deep Fij restarted") {
      "Deep Fij was restarted on %s at %s.".format(InetAddress.getLocalHost.getHostName, new LocalDateTime().toString("yyyy-MM-dd HH:mm:ss"))
    }
  }


  def emailAdmin(subject: String)(f: => String) {
    import com.typesafe.plugin._
    try {
      val mail = use[MailerPlugin].email
      mail.setSubject(subject)
      //TODO make that a config setting
      mail.setRecipient("fijimf@gmail.com")
      mail.setFrom("Deep Fij <deepfij@gmail.com>")
      mail.send(f)
    }
    catch {
      case NonFatal(e) => logger.error("CaughtException trying to send mail.", e)
    }
  }
}
