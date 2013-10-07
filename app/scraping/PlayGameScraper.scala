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

  def loadGames(d: Date): Future[List[(String, String, String, String, String)]] = {
    val year = new SimpleDateFormat("yyyy").format(d)
    val month = new SimpleDateFormat("MM").format(d)
    val day = new SimpleDateFormat("dd").format(d)

    val url = "http://data.ncaa.com/jsonp/scoreboard/basketball-men/d1/" + year + "/" + month + "/" + day + "/scoreboard.html"

    loadUrl(url).map((response: Response) => {
      response.body
    def parseGameResponse(s: String): Option[GameResponse] = {
      val j = s.replaceFirst( """^callbackWrapper\(\{""", "{").replaceFirst( """\}\)$""", "}").replaceAll( """,[\s+,]+""", ",").replaceAll( """,\s*\]""", "]")
      try {
        Some(parse[GameResponse](j))
      } catch {
        case e: Exception => {
          println(d + " " + e.getMessage)
          e.printStackTrace()
          println(j)
          None
        }
      }
    }
    try {
      h(url(req) >- parseGameResponse _)
    } catch {
      case e: Exception => {
        println(d + ": Caught " + e.getMessage)
        None
      }
    } finally {
      h.shutdown()
    }
  }


  def gameList(date: Date): List[(String, Option[Int], String, Option[Int])] = {
    (for (gr <- loadDateGames(date).toList;
          sb <- gr.scoreboard;
          g <- sb.games) yield {
      val (homeTeam, homeScore) = teamData(g.home)
      val (awayTeam, awayScore) = teamData(g.away)
      (homeTeam, homeScore, awayTeam, awayScore)
    }).toList
  }

  def teamData(t: Team): (String, Option[Int]) = {
    val name = teams.getOrElse(t.key, t.name)
    val score = t.scoreBreakdown match {
      case Nil => None
      case xs => Some(xs.map(_.toInt).sum)
    }
    (name, score)
  }
}
}
