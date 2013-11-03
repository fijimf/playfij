package scraping

import org.joda.time.LocalDate
import scala.concurrent.Future
import play.api.Logger
import java.text.SimpleDateFormat
import scala.util.control.Exception._
import scala.Some
import play.api.libs.ws.Response
import play.api.libs.concurrent.Execution.Implicits._


object KenPomGameScraper extends AbstractGameScraper {
  def logger = Logger(this.getClass.getName)

  def fetch(req: GameUpdateRequest): Future[List[ResultData]] = {
    scrapeKenPom(req.url)
  }

  def scrapeKenPom(url: String): Future[List[ResultData]] = {
    val inFormat = new SimpleDateFormat("MM/dd/yyyy")
    loadUrl(url).map((response: Response) => {
      response.body.split("\n").filter(_.length > 63).map(s => {
        val d = new LocalDate(inFormat.parse(s.substring(0, 10)).getTime)
        val at = s.substring(11, 33).trim()
        val as = catching(classOf[NumberFormatException]) opt s.substring(34, 37).trim().toInt
        val ht = s.substring(38, 60).trim()
        val hs = catching(classOf[NumberFormatException]) opt s.substring(61, 64).trim().toInt
        (hs, as) match {
          case (Some(h), Some(a)) => ResultData(GameData(d, ht, at), Some((h, a)))
          case _ => ResultData(GameData(d, ht, at), None)
        }
      })
        .toList
    })
  }
}


