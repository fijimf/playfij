package analysis

import models.ScheduleData
import org.joda.time.LocalDate
import scala.Predef._

case class AccumulativeStatistic[V](fs: Map[String, (V) => Double], accumulate: (ScheduleData, Map[Long, V]) => Map[Long, V], data: Map[Long, V] = Map.empty[Long, V]) {

  def handleObs(totals: Map[String, Map[LocalDate, Map[Long, Double]]], obs: ScheduleData): (AccumulativeStatistic[V], Map[String, Map[LocalDate, Map[Long, Double]]]) = {
    val a: Map[Long, V] = accumulate(obs, data)
    val out = fs.foldLeft(totals) {
      case (d: Map[String, Map[LocalDate, Map[Long, Double]]], funkey: (String, (V) => Double)) => {
        val (key, fun) = funkey
        d + (key -> (d.getOrElse(key, Map.empty[LocalDate, Map[Long, Double]]) + (obs.game.date -> a.mapValues(fun))))
      }
    }
    (copy(data = a), out)
  }
}

