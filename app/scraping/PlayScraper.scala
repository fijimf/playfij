package scraping

import play.api.Logger
import scala.concurrent.Future
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._

trait PlayScraper {
  def logger: Logger

  def loadUrl(url: String): Future[Response] = {
    withLatency(WS.url(url).get).map((tuple: (Long, Response)) => {
      logger.info("Received %s in %d ms.".format(tuple._1, tuple._2))
      tuple._2
    })
  }

  def withLatency[T](f: Future[T]): Future[(Long, T)] = {
    val startTime = System.currentTimeMillis()
    f.map((System.currentTimeMillis() - startTime, _))
  }
  
  //TODO add withCache
  
  //TODO ddd withRetry
  
}
