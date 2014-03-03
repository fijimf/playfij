package controllers

import play.api.Logger

object Util {
  val logger = Logger("Timing")

  def timed[T](id: String)(block: => T):T= {
    val start = System.currentTimeMillis()
    val t = block
    val end = System.currentTimeMillis()
    logger.info("%s => %d ms.".format(id, end - start))
    t
  }


}
