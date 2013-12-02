package analysis

import org.joda.time.LocalDate
import scala.Predef._
import models.ScheduleData

trait AccumulatorModel extends ComputableModel {
  def accumulators: List[AccumulativeStatistic[_]]

  def computeSeason(data: List[ScheduleData]) = {
    val results: List[ScheduleData] = data.filter(_.result.isDefined).sortBy(_.game.date.toDate)
    val zero = (accumulators, Map.empty[String, Map[LocalDate,Map[Long,Double]]])
    results.foldLeft(zero)(handleObs)._2
  }

  def handleObs(outer: (List[AccumulativeStatistic[_]], Map[String, Map[LocalDate, Map[Long, Double]]]), obs: ScheduleData) = {
    val (accumulators, data) = outer
    val zero: (List[AccumulativeStatistic[_]], Map[String, Map[LocalDate, Map[Long, Double]]]) = (List.empty[AccumulativeStatistic[_]], data)
    accumulators.foldLeft(zero) {
      case (inner: (List[AccumulativeStatistic[_]], Map[String, Map[LocalDate, Map[Long, Double]]]), accumulator: AccumulativeStatistic[_]) => {
        val (newAccs, newTotals) = inner
        val tup: ((AccumulativeStatistic[_], Map[String, Map[LocalDate, Map[Long, Double]]])) = accumulator.handleObs(newTotals, obs)
        (tup._1 :: newAccs, tup._2)
      }
    }
  }

}
