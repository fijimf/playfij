package scraping

import xml.{NodeSeq, Node}
import collection.immutable.{Map, List}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import play.Logger

object NcaaTeamScraper extends HttpScraper {

  object TeamData {
    val Key = "key"
    val Name = "name"
    val ConferenceName = "conference"
    val LongName = "longName"
    val Nickname = "nickname"
    val PrimaryColor = "primaryColor"
    val SecondaryColor = "secondaryColor"
    val OfficialUrl = "officialUrl"
    val LogoUrl = "logo"
  }

  val logger = Logger.of(NcaaTeamScraper.getClass)


  lazy val teamData: List[Map[String, String]] = {
    logger.info("Scraping NCAA.com for teams and conferences.")
    val teamKeys: Map[String, String] = allNcaaTeams.map {
      case (k: String, n: String) => (k.replaceAll("--", "-"), n) // Fix for stupid NCAA.com data
    }
    val parKeys = teamKeys.keys.toSet.par
    parKeys.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(24))
    logger.info(shortNames.size + " short names")
    val teamsRaw = parKeys.map(k => {
      teamDetail(k, shortNames.getOrElse(k, teamKeys(k)), teamKeys(k))
    }).seq.flatten
    logger.info("Loaded " + teamsRaw.size + " teams")
    //Weird fix for teams moving up
    val teams = teamsRaw.map(t => {
      if (t.name.toLowerCase.replaceAll("\\W", "").take(12) == t.conference.toLowerCase.replaceAll("\\W", "").take(12)) {
        t.copy(conference = "Independent")
      } else {
        t
      }
    })
    teams.map(tr => {
      List(
        Some(TeamData.Key -> tr.keyName),
        Some(TeamData.Name -> tr.name),
        Some(TeamData.LongName -> tr.fullName),
        Some(TeamData.ConferenceName -> tr.conference),
        tr.nickname.map((TeamData.Nickname -> _)),
        tr.logo.map((TeamData.LogoUrl -> _)),
        tr.primaryColor.map((TeamData.PrimaryColor -> _)),
        tr.secondaryColor.map((TeamData.SecondaryColor -> _)),
        tr.officialUrl.map((TeamData.OfficialUrl -> _))
      ).flatten.toMap
    }).toList
  }

  lazy val conferenceMap = teamData.map(_(TeamData.ConferenceName)).map(c => (c.replaceFirst(" Conference$", "").replaceFirst(" League$", "").replaceFirst("^The ", "").replaceAll("\\.", "").toLowerCase.replace(' ', '-') -> c)).toMap

  private[this] lazy val allNcaaTeams: Map[String, String] = {
    logger.info("Loading canonical team names")
    val pairs: Seq[(String, String)] = "abcdefghijklmnopqrstuvwxyz".par.map((c: Char) => {
      val  pageXml = loadURL("http://www.ncaa.com/schools/" + c + "/")

      logger.info("Loaded '" + c + "' => " + ("http://www.ncaa.com/schools/" + c + "/"))
      scrapeAlphaTeamsPage(pageXml)
    }).flatten.seq
    pairs.toMap
  }


  def scrapeAlphaTeamsPage(pageXml: Node): Seq[(String, String)] = {
    val teamNodes = (pageXml \\ "span").filter((node: Node) => (node \ "@class").text == "field-content")
    val pageData = teamNodes.map((node: Node) => (node \ "a").map((node: Node) => {
      ((node \ "@href").text.split("/").last -> node.text)
    })).flatten
    logger.info(pageData.mkString(","))
    pageData
  }

  lazy val shortNames: Map[String, String] = {
    logger.info("Loading short names")

    List("p1", "p2", "p3", "p4", "p5", "p6", "p7").par.map(t => loadURL("http://www.ncaa.com/stats/basketball-men/d1/current/team/145/" + t)).map((page: Node) => {
      (page \\ "a").filter((node: Node) => (node \ "@href").text.startsWith("/schools/")).map((node: Node) => {
        ((node \ "@href").text.replace("/schools/", "") -> node.text)
      })
    }).flatten.seq.toMap
  }

  def teamDetail(key: String, name: String, longName: String): Option[TeamRecord] = {
    val page: Node = loadURL("http://www.ncaa.com/schools/" + key)
    val conference: Option[String] = parseConference(page)
    if (conference.isDefined) {
      logger.info("Found " + key)
      Some(parseDetails(page, TeamRecord(key, conference.get, name, longName)).copy(logo = parseLogoUrl(page)))
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

  private[this] def parseDetails(page: Node, r: TeamRecord): TeamRecord = {
    val nicknameKey = "Nickname"
    val colorsKey = "Colors"
    val urlKey = "Url"
    val detailMap: Map[String, String] = (page \\ "td").map((node: Node) => node match {
      case <td><h6>Nickname</h6><p>{nickname}</p></td> => Some((nicknameKey -> nickname.text))
      case <td><h6>Athletics Website</h6><p><a>{url}</a></p></td> => Some((urlKey -> url.text))
      case <td><h6>Colors</h6><p>{colors}</p></td> => Some((colorsKey -> colors.text))
      case _ => None }).flatten.toMap
    val optColors = detailMap.get(colorsKey)
    if (optColors.isDefined) {
      val carr: Array[String] = optColors.get.trim.split('&')
      if (carr.length > 1) {
        r.copy(nickname = detailMap.get(nicknameKey),
          primaryColor = Some(carr(0).trim),
          secondaryColor = Some(carr(1).trim),
          officialUrl = detailMap.get(urlKey))

      } else {
        r.copy(nickname = detailMap.get(nicknameKey),
          primaryColor = Some(carr(0).trim),
          secondaryColor = None,
          officialUrl = detailMap.get(urlKey))
      }
    } else {
      r.copy(nickname = detailMap.get(nicknameKey),
        primaryColor = None,
        secondaryColor = None,
        officialUrl = detailMap.get(urlKey))
    }
  }


  case class TeamRecord(keyName: String,
                        conference: String,
                        name: String,
                        fullName: String,
                        nickname: Option[String] = None,
                        primaryColor: Option[String] = None,
                        secondaryColor: Option[String] = None,
                        logo: Option[String] = None,
                        officialUrl: Option[String] = None)

  def main(args: Array[String]) {
    teamData.foreach(println(_))
  }
}

