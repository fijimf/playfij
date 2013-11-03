package scraping

import scala.concurrent.{Await, Future}
import scala.xml.Node
import models.Team
import play.api.libs.concurrent.Execution.Implicits._
import scraping.util.HtmlUtil._
import scala.concurrent.duration._
import play.api.Logger

object NcaaTeamScraper extends AbstractScraper{
  val logger = Logger(this.getClass.getName)

  def teamRawData(): List[(String, Team)] = {
    val longNames: Map[String, String] = Await.result(loadLongNames(), 2.minutes)
    logger.info("Loaded %d long names.".format(longNames.size))
    val shortNames: Map[String, String] = Await.result(loadShortNames(), 2.minutes)
    logger.info("Loaded %d short names".format(shortNames.size))

    val teamDetailBatchSize: Int = 50
    val result = shortNames.grouped(teamDetailBatchSize).map(m => {
      logger.info("Processing 30 teams...")
      processBatch(m, longNames)
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

  def processBatch(shortNames: Map[String, String], longNames: Map[String, String]): List[(String, Team)] = {
    logger.info("Batching team details (batch size=%d ".format(shortNames.size))
    val iterable: Iterable[Future[Option[(String, Team)]]] = for (k <- shortNames.keys) yield {
      val k1 = k.replaceAll("--", "-")
      teamDetail(k1, Some(shortNames(k))).recover {
        case thr: Throwable => {
          logger.error("Failed loading %s".format(k))
          None
        }
      }
    }
    Await.result(Future.sequence(iterable).map(_.flatten.toList), 7.minutes)
  }

  def loadLongNames(): Future[Map[String, String]] = {
    Future.sequence("abcdefghijklmnopqrstuvwxyz".map((c: Char) => {
      loadUrl("http://www.ncaa.com/schools/" + c + "/").map(r => {
        val longNames: Seq[(String, String)] = teamNamesFromAlphaPage(loadHtmlFromString(r.body).get)
        logger.info("For the letter %s, %d long names retrieved".format(c, longNames.size))
        longNames
      })
    })).map(_.flatten).map(_.toMap)
  }

  def loadShortNames(): Future[Map[String, String]] = {
    Future.sequence(List("p1", "p2", "p3", "p4", "p5", "p6", "p7").map(t => {
      loadUrl("http://www.ncaa.com/stats/basketball-men/d1/current/team/145/" + t).map(r => {
        val shortNames: Seq[(String, String)] = teamNamesFromStatPage(loadHtmlFromString(r.body).get)
        logger.info("For the page %s, %d long names retrieved".format(t, shortNames.size))
        shortNames
      })
    })).map(_.flatten).map(_.toMap)
  }


  def teamNamesFromAlphaPage(node: Node):Seq[(String, String)] = {
    val schoolList: Option[Node] = (node \\ "div").find(n => attrMatch(n, "id", "school-list")).flatMap(_.headOption)
    extractNamesAndKeys(schoolList).toSeq
  }
  
  def teamNamesFromStatPage(node: Node):Seq[(String, String)] = {
    val schoolList: Option[Node] = (node \\ "div").find(n => attrMatch(n, "class", "ncaa-stat-category-stats")).flatMap(_.headOption)
    extractNamesAndKeys(schoolList).toSeq
  }

  /**
   *
   * @param schoolList HTML containing links of the form &lt;a href="school-key"&gt;School Name&lt;/a&gt;
   * @return A list of team key and team name pairs.
   */
  def extractNamesAndKeys(schoolList: Option[Node]): Iterator[(String, String)] = {
    for (d <- schoolList.iterator;
         link <- d \\ "a";
         href <- attrValue(link, "href") if href.startsWith("/schools/"))
    yield {
      href.substring(9) -> link.text
    }
  }

  def teamDetail(key: String, shortName: Option[String]): Future[Option[(String, Team)]] = {
    loadUrl("http://www.ncaa.com/schools/" + key).map(resp => {
      loadHtmlFromString(resp.body).map(node => {
        val longName = schoolName(node).getOrElse(shortName.getOrElse(key.replaceAll("-"," ").capitalize))
        val metaInfo = schoolMetaInfo(node)
        val nickname = metaInfo.getOrElse("nickname", "MISSING")
        val primaryColor = schoolPrimaryColor(node)
        val secondaryColor = primaryColor.map(c => desaturate(c, 0.4))
        val logoUrl = schoolLogo(node)
        val officialUrl = schoolOfficialWebsite(node)
        val officialTwitter = schoolOfficialTwitter(node)
        val conference = metaInfo.getOrElse("conf", "MISSING")
        (conference, Team(0, key, shortName.getOrElse(longName), longName, nickname, primaryColor, secondaryColor, logoUrl, officialUrl, officialTwitter))
      })
    })
  }

  def schoolName(n: Node): Option[String] = {
    (n \\ "span").find(n => attrMatch(n, "class", "school-name")).map(_.text)
  }

  def schoolLogo(n: Node): Option[String] = {
    (n \\ "span").find(n => attrMatch(n, "class", "school-logo")).map(_ \\ "img").flatMap(_.headOption).flatMap(nn=>attrValue(nn,"src"))
  }

  def schoolPrimaryColor(n: Node): Option[String] = {
    (n \\ "span").find(n => attrMatch(n, "class", "school-logo")).flatMap(nn=>attrValue(nn,"style")).map(_.replaceFirst("border-color:","").replace(";","").trim)
  }
  def schoolOfficialWebsite(n: Node): Option[String] = {
    (n \\ "li").find(n => attrMatch(n, "class", "school-social-website")).map(_ \\ "a").flatMap(_.headOption).flatMap(nn=>attrValue(nn,"href"))
  }
  def schoolOfficialTwitter(n: Node): Option[String] = {
    (n \\ "li").find(n => attrMatch(n, "class", "school-social-twitter")).map(_ \\ "a").flatMap(_.headOption).flatMap(nn=>attrValue(nn,"href"))
  }

  def schoolMetaInfo(n:Node):Map[String,String] = {
    val items: Seq[Node] = (n \\ "li").filter(n => attrMatch(n, "class", "school-info"))
    items.foldLeft(Map.empty[String, String])((m:Map[String, String], i:Node)=>{
      val key = (i \ "span").text.trim
      val value = i.text.replace(key,"").trim
      m+(key.toLowerCase.replaceAll("\\W","")->value)
    })
  }

  def desaturate(c:String, a:Double):String = {
    val r = Integer.parseInt(c.substring(1,3),16)
    val g = Integer.parseInt(c.substring(3,5),16)
    val b = Integer.parseInt(c.substring(5,7),16)
    "rgba( %d, %d, %d, %f)".format(r,g,b,a)
  }
}


