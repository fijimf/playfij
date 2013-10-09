package controllers.admin

import play.api.mvc.Controller
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import models.Repository

object KenPom extends Controller {

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  case class KenPomUpdateRequest(
      url:String, 
      doWrite:boolean,
	  doGameInserts:boolean,
	  doGameUpdates:boolean,
	  doGameDeletes:boolean,
	  doResultInserts:boolean,
	  doResultUpdates:boolean,
	  doResultDeletes:boolean,
	  fromDate:Option[LocalDate],
	  toDate:Option[LocalDate]
  )  
  
  val form: Form[KenPomUpdateRequest] = Form(
    mapping(
      "url" -> nonEmptyText,
      "doWrite" -> boolean,
	  "doGameInserts" -> boolean,
	  "doGameUpdates" -> boolean,
	  "doGameDeletes" -> boolean,
	  "doResultInserts" -> boolean,
	  "doResultUpdates" -> boolean,
	  "doResultDeletes" -> boolean,
	  "fromDate" -> optionalText,
	  "toDate" -> optionaText,
    )((url, writeToDb) => (url, writeToDb))
      (tup => Some(tup))
  )
  
  case class KenPomUpdateResult(
    badFormat:List[String] = List.empty(),
	unknownTeam:List[String] = List.empty(),
	noSeason:List[String] = List.empty(),
	outsideRange:List[String] = List.empty(),
	gamesInserted:List[(LocalDate,String, String)] = List.empty(),
	gamesUpdated:List[(LocalDate,String, String)] = List.empty(),
	gamesDeleted:List[(LocalDate,String, String)] = List.empty(),
	resultsInserted:List[(LocalDate,String, String, Int, Int)] = List.empty(),
	resultsUpdated:List[(LocalDate,String, String, Int, Int)] = List.empty(),
	resultsDeleted:List[(LocalDate,String, String, Int, Int)] = List.empty()
  )
  
  def index = Action {
    implicit request =>
        Ok(views.html.kenpom(form.blank))
      }
  }

  def scrapeGames = Action {
      
  
  
  }




}
