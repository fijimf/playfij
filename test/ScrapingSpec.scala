import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import scraping.NcaaTeamScraper

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class ScrapingSpec extends Specification {

  "The 'Hello world' string" should {
    "contain 11 characters" in {
      "Hello world" must have size(11)
    }
    "start with 'Hello'" in {
      "Hello world" must startWith("Hello")
    }
    "end with 'world'" in {
      "Hello world" must endWith("world")
    }

    "not blow up " in {
      val names: Map[String, String] = NcaaTeamScraper.shortNames
      println(names)
      names  mustNotEqual (Map.empty[String, String])
    }
  }
}