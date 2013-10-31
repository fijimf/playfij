package scraping

import play.api.libs.ws.{Response, WS}
import scala.concurrent.{Await, Future}
import scala.xml.{PrettyPrinter, Elem, NodeSeq, Node}
import models.Team
import play.api.libs.concurrent.Execution.Implicits._
import HtmlHelper.loadHtmlFromString
import scala.concurrent.duration._
import play.api.Logger

object PlayTeamScraper extends PlayScraper{
  val logger = Logger(this.getClass.getName)

  def teamRawData(): List[(String, Team)] = {
    val teamNames: Map[String, String] = Await.result(loadTeamNames(), 2.minutes)
    logger.info("Loaded team names")
    val shortName: Map[String, String] = Await.result(loadShortNames(), 2.minutes)
    logger.info("Loaded short names")

    val teamDetailBatchSize: Int = 50
    val result = teamNames.grouped(teamDetailBatchSize).map(m => {
      logger.info("Processing 30 teams...")
      processBatch(m, shortName)
    }).flatten.toList
    logger.info("Loaded %d teams from the NCAA website".format(result.size))
    result.map {
      case (conf: String, team: Team) => {
        if (team.name.toLowerCase.replaceAll("\\W", "").take(12) == conf.toLowerCase.replaceAll("\\W", "").take(12)) {
          ("Independent", team)
        } else {
          (conf, team)
        }
      }
    }.toList
  }


  def processBatch(teamNames: Map[String, String], shortNames: Map[String, String]): List[(String, Team)] = {
    logger.info("Batching team details (batch size=%d ".format(shortNames.size));
    val iterable: Iterable[Future[Option[(String, Team)]]] = for (k <- teamNames.keys) yield {
      val k1 = k.replaceAll("--", "-")
      teamDetail(k1, shortNames.get(k).getOrElse(teamNames(k)), teamNames(k)).recover {
        case thr: Throwable => {
          logger.error("Failed loading %s".format(k))
          None
        }
      }
    }
    val result: List[(String, Team)] = Await.result(Future.sequence(iterable).map(_.flatten.toList), 7.minutes)
    logger.info("Here returning result " + result.size);
    result
  }

  lazy val teamList: List[Team] = {
    teamRawData().map(_._2)
  }

  def loadTeamNames(): Future[Map[String, String]] = {
    Future.sequence("abcdefghijklmnopqrstuvwxyz".map((c: Char) => {
      loadUrl("http://www.ncaa.com/schools/" + c + "/").map(r => scrapeAlphaTeams(loadHtmlFromString(r.body).get))
    })).map(_.flatten).map(_.toMap)
  }

  def scrapeAlphaTeams(pageXml: Node): Seq[(String, String)] = {
    val sb= new StringBuilder()
    new PrettyPrinter(120,2).format(pageXml,sb)
    logger.info(sb.toString())
    val teamNodes = (pageXml \\ "div").filter(n=> attrEquals(n,"id","team-list"))
    logger.info("DIVS:"+teamNodes.length)
    val pageData = teamNodes.map((node: Node) => (node \ "a").map((node: Node) => {
      (node \ "@href").text.split("/").last -> node.text
    })).flatten
    pageData
  }

  def attrEquals(node: Node, attr:String, value: String):Boolean = {

    node.attribute(attr) match {
      case Some(seqNode) => seqNode.exists(n=>n.text == value)
      case _ => false
    }
  }

  def loadShortNames(): Future[Map[String, String]] = {
    Future.sequence(List("p1", "p2", "p3", "p4", "p5", "p6", "p7").map(t => {
      WS.url("http://www.ncaa.com/stats/basketball-men/d1/current/team/145/" + t).get().map(r => {
        (loadHtmlFromString(r.body).get \\ "a").filter((node: Node) => (node \ "@href").text.startsWith("/schools/")).map((node: Node) => {
          (node \ "@href").text.replace("/schools/", "") -> node.text
        })
      })
    })).map(_.flatten).map(_.toMap)
  }

  def teamDetail(key: String, name: String, longName: String): Future[Option[(String, Team)]] = {
    loadUrl("http://www.ncaa.com/schools/" + key).map(resp => {
      val node = loadHtmlFromString(resp.body).get

      parseConference(node) match {
        case Some(conference) => {
          val team = parseDetails(node, Team(0, key, name, longName, "Missing", None, None, None, None, None)).copy(logoUrl = parseLogoUrl(node))
          Some(conference, team)
        }
        case None => None
      }
    })
  }

  def parseConference(page: Node): Option[String] = {
    val tableRow: NodeSeq = (page \\ "tr").filter((node: Node) => {
      val seq: NodeSeq = node \ "td"
      if (seq.size == 3) {
        ((seq.head \ "a" \ "@href").text.endsWith("basketball-men") && seq.tail.head.text == "DI")
      } else {
        false
      }
    })
    if (tableRow.isEmpty) {
      None
    } else {
      Some((tableRow \ "td").last.text)
    }
  }

  private[this] def parseLogoUrl(page: Node): Option[String] = {
    (page \\ "img").filter((node: Node) => (node \ "@class").text == "school-logo").headOption.map((node: Node) => (node \ "@src").text)
  }

  private[this] def parseDetails(page: Node, r: Team): Team = {
    val nicknameKey = "Nickname"
    val colorsKey = "Colors"
    val urlKey = "Url"
    val detailMap: Map[String, String] = (page \\ "td").map((node: Node) => node match {
      case <td><h6>Nickname</h6><p>{nickname}</p></td> => Some(nicknameKey -> nickname.text)
      case <td><h6>Athletics Website</h6><p><a>{url}</a></p></td> => Some(urlKey -> url.text)
      case <td><h6>Colors</h6><p>{colors}</p></td> => Some(colorsKey -> colors.text)
      case _ => None
    }).flatten.toMap
    val optColors = detailMap.get(colorsKey)
    if (optColors.isDefined) {
      val carr: Array[String] = optColors.get.trim.split('&')
      if (carr.length > 1) {
        r.copy(nickname = detailMap.get(nicknameKey).getOrElse("Missing"),
          primaryColor = Some(carr(0).trim),
          secondaryColor = Some(carr(1).trim),
          officialUrl = detailMap.get(urlKey))

      } else {
        r.copy(nickname = detailMap.get(nicknameKey).getOrElse("Missing"),
          primaryColor = Some(carr(0).trim),
          secondaryColor = None,
          officialUrl = detailMap.get(urlKey))
      }
    } else {
      r.copy(nickname = detailMap.get(nicknameKey).getOrElse("Missing"),
        primaryColor = None,
        secondaryColor = None,
        officialUrl = detailMap.get(urlKey))
    }
  }
}


