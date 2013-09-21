package scraping

import play.api.libs.ws.{WS}
import scala.concurrent.Future
import scala.xml.{NodeSeq, Node}
import models.Team

class PlayScraper {

  lazy val teamRawData: List[(String, Team)] = {
    val teamsRaw = for (mtn <- loadTeamNames();
                        sn <- loadShortNames;) {
      mtn.map {
        case (k: String, n: String) => {
          val k1 = k.replaceAll("--", "-")
          teamDetail(k1, sn.getOrElse(k1, n), n)
        }
      }
    }

    teamsRaw.map( {
      case (conf: String, team: Team) => {
        if (team.name.toLowerCase.replaceAll("\\W", "").take(12) == conf.toLowerCase.replaceAll("\\W", "").take(12)) {
          ("Independent", team)
        } else {
          (conf, team)
        }

      }
    }.toList)
  }

  lazy val teamList: List[Team] = {
    teamRawData.map(_._2)
  }

  def loadTeamNames(): Future[Map[String, String]] = {
    Future.sequence("abcdefghijklmnopqrstuvwxyz".map((c: Char) => {
      WS.url("http://www.ncaa.com/schools/" + c + "/").get().map(r => scrapeAlphaTeams(r.xml))
    })).map(_.flatten).map(_.toMap)
  }

  def scrapeAlphaTeams(pageXml: Node): Seq[(String, String)] = {
    val teamNodes = (pageXml \\ "span").filter((node: Node) => (node \ "@class").text == "field-content")
    val pageData = teamNodes.map((node: Node) => (node \ "a").map((node: Node) => {
      (node \ "@href").text.split("/").last -> node.text
    })).flatten
    pageData
  }

  def loadShortNames: Future[Map[String, String]] = {
    Future.sequence(List("p1", "p2", "p3", "p4", "p5", "p6", "p7").map(t => {
      WS.url("http://www.ncaa.com/stats/basketball-men/d1/current/team/145/" + t).get().map(r => {
        (r.xml \\ "a").filter((node: Node) => (node \ "@href").text.startsWith("/schools/")).map((node: Node) => {
          (node \ "@href").text.replace("/schools/", "") -> node.text
        })
      })
    })).map(_.flatten).map(_.toMap)
  }

  def teamDetail(key: String, name: String, longName: String): Option[(String, Team)] = {
    val page: Node = loadURL("http://www.ncaa.com/schools/" + key)
    val conference: Option[String] = parseConference(page)
    if (conference.isDefined) {
      logger.info("Found " + key)
      Some(
        (conference.get,
          parseDetails(page, Team(0, key, name, longName, "Missing", None, None, None, None, None)).copy(logoUrl = parseLogoUrl(page)))
      )
    } else {
      None
    }
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
      case _ => None }).flatten.toMap
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
