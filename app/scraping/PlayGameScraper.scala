package scraping

import scala.util.control.Exception._
import play.api.libs.json._
import play.api.Logger
import java.text.SimpleDateFormat
import play.api.libs.ws.Response
import scala.concurrent.Future
import org.joda.time.LocalDate
import play.api.libs.concurrent.Execution.Implicits._
import scala.annotation.tailrec
import org.apache.commons.lang3.StringUtils
import scala.io.Codec

object PlayGameScraper extends PlayScraper {
  def logger = Logger("GameScraper")

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

  def loadGames(from: LocalDate, to: LocalDate, accum: Future[List[ResultData]]): Future[List[ResultData]] = {
    val gf: Future[List[ResultData]] = loadGames(from).flatMap(gs => accum.map(_ ::: gs))
    if (from == to) {
      gf
    } else {
      loadGames(from.plusDays(1), to, gf)
    }
  }

  def loadGames(from: LocalDate, to: LocalDate): Future[List[ResultData]] = {
    val zero: Future[List[ResultData]] = Future(List.empty[ResultData])
    loadGames(from, to, zero)
  }

  def loadGames(d: LocalDate): Future[List[ResultData]] = {
    val year = new SimpleDateFormat("yyyy").format(d.toDate)
    val month = new SimpleDateFormat("MM").format(d.toDate)
    val day = new SimpleDateFormat("dd").format(d.toDate)

    val url = "http://data.ncaa.com/jsonp/scoreboard/basketball-men/d1/" + year + "/" + month + "/" + day + "/scoreboard.html"

    loadUrl(url).map((response: Response) => {
      ripGames(response.body, d)
    })
  }


  def ripJson[T](json: String, f: (JsValue => T)): List[T] = {
    if (StringUtils.isBlank(json)) {
      List.empty
    } else {
      try {
        val parse: JsValue = Json.parse(stripCallbackWrapper(json))
        val games = ((parse \ "scoreboard")(0) \ "games").validate[JsArray] match {
          case JsSuccess(g, _) => g.asInstanceOf[JsArray].value.toList
          case _ => {
            logger.error("Error parsing Json")
            List.empty
          }
        }
        games.map(f)
      } catch {
        case t: Throwable => {
          logger.error("Error parsing Json", t)
         // logger.error(json)
          List.empty
        }
      }
    }
  }

  def ripGames(json: String, date: LocalDate): List[ResultData] = {
    ripJson(json, ripGameResult).flatten.map(tup => ResultData(GameData(date, tup._1, tup._2), tup._3))
  }

  val ripTwitters: ((String, JsValue) => Option[(String, String)]) = {
    case (ha, j) =>
      val t = (j \ ha \ "nameSeo").asOpt[String]
      val bb = (j \ ha \ "social" \ "twitter" \ "accounts" \ "sport").asOpt[String]
      val ad = (j \ ha \ "social" \ "twitter" \ "accounts" \ "athleticDept").asOpt[String]
      val cf = (j \ ha \ "social" \ "twitter" \ "accounts" \ "conference").asOpt[String]
      (t, bb.orElse(ad).orElse(cf)) match {
        case (Some(team), Some(twitter)) => Some(team -> twitter)
        case _ => None
      }
  }

  def ripTwitterMap(json: String): Map[String, String] = {
    (ripJson(json, ripTwitters("home", _)).flatten ++ ripJson(json, ripTwitters("away", _)).flatten).toMap
  }

  def ripColors(json: String): Map[String, String] = {
    Map.empty[String, String]
  }

  val ripGameResult: (JsValue => Option[(String, String, Option[(Int, Int)])]) = {
    j =>
      val result = (
        teamData(j, "home", "currentScore").flatMap(x => catching(classOf[NumberFormatException]) opt x.toInt),
        teamData(j, "away", "currentScore").flatMap(x => catching(classOf[NumberFormatException]) opt x.toInt)
        ) match {
        case (Some(h), Some(a)) => Some(h.asInstanceOf[Int], a.asInstanceOf[Int])
        case _ => None
      }

      val teams = (teamData(j, "home", "nameSeo"), teamData(j, "away", "nameSeo")) match {
        case (Some(h), Some(a)) => Some(h.toString, a.toString, result)
        case _ => None
      }
      teams
  }

  def teamData(game: JsValue, homeOrAway: String, item: String): Option[String] = {
    (game \ homeOrAway \ item).asOpt[String]
  }

  def stripCallbackWrapper(json: String): String = {
    json
      .replaceFirst( """^callbackWrapper\(\{""", """{""")
      .replaceFirst( """}\);$""", """}""")
      .replaceAll( """,\s+,""", ", ")
  }
}
