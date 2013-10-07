package scraping

import play.api.Logger
import java.text.SimpleDateFormat
import play.api.libs.ws.Response
import scala.concurrent.Future

object PlayGameScraper extends PlayScraper {
  def logger = Logger("GameScraper")

  def scrapeKenPom(url: String): Future[List[(String, String, String, String, String)]] = {
    val inFormat = new SimpleDateFormat("MM/dd/yyyy")
    val outFormat = new SimpleDateFormat("yyyyMMdd")
    loadUrl(url).map((response: Response) => {
      response.body.split("\n").filter(_.length > 63).map(s => {
        val d = outFormat.format(inFormat.parse(s.substring(0, 10)))
        val at = s.substring(11, 33).trim()
        val as = s.substring(34, 37).trim()
        val ht = s.substring(38, 60).trim()
        val hs = s.substring(61, 64).trim()
        (d, ht, hs, at, as)
      })

    }.toList)
  }

}
