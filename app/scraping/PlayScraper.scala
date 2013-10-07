package scraping

/**
 * Created with IntelliJ IDEA.
 * User: Jim
 * Date: 10/6/13
 * Time: 10:41 PM
 * To change this template use File | Settings | File Templates.
 */
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
}
