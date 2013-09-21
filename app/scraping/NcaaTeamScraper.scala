package scraping

import xml.{NodeSeq, Node}
import collection.immutable.{Map, List}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import play.api.Logger
import models.Team

object NcaaTeamScraper extends HttpScraper {

  val logger = Logger("NcaaTeamScraper")

  lazy val teamRawData:List[(String, Team)] = {
    logger.info("Scraping NCAA.com for teams and conferences.")
    val teamKeys: Map[String, String] = allNcaaTeams.map {
      case (k: String, n: String) => (k.replaceAll("--", "-"), n) // Fix for stupid NCAA.com data
    }
    val parKeys = teamKeys.keys.toSet
//    parKeys.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(24))
    logger.info(shortNames.size + " short names")
    val teamsRaw: Set[(String, Team)] = parKeys.map(k => {
      teamDetail(k, shortNames.getOrElse(k, teamKeys(k)), teamKeys(k))
    })flatten

    logger.info("Loaded " + teamsRaw.size + " teams")
    //Weird fix for teams moving up
    teamsRaw.map{case (conf: String, team: Team) => {
      if (team.name.toLowerCase.replaceAll("\\W", "").take(12) == conf.toLowerCase.replaceAll("\\W", "").take(12)) {
        ( "Independent", team)
      } else {
        (conf, team)
      }

    }}.toList
  }

  lazy val teamList: List[Team] = {
    teamRawData.map(_._2)
  }

  lazy val conferenceMap:Map[String, String] = teamRawData.map(_._1).map(c => (c.replaceFirst(" Conference$", "").replaceFirst(" League$", "").replaceFirst("^The ", "").replaceAll("\\.", "").toLowerCase.replace(' ', '-') -> c)).toMap

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
        (node \ "@href").text.replace("/schools/", "") -> node.text
      })
    }).flatten.seq.toMap
  }

}

