package analysis.predictors

import org.saddle.{Series, Frame}
import org.joda.time.LocalDate
import models.{Statistic, ScheduleData, Team}
import org.apache.mahout.math.{DenseVector, Vector}
import org.saddle.scalar.Scalar
import scala.collection.immutable.Iterable

case class MultiStatFeatureMapper(stats: List[(Statistic, Frame[LocalDate, Team, Double])]) extends FeatureMapper {

  override def featureDimension: Int = stats.size + 1

  override def featureName(i:Int): String ={
     if (i==0) {
       "Constant"
     } else {
       stats(i-1)._1.name
     }
  }

  override def feature(obs: ScheduleData): Option[Vector] = {
    if (obs.result.isDefined) {
      val values = stats.map {
        case (statistic: Statistic, frame: Frame[LocalDate, Team, Double]) =>
          if (frame.rowIx.contains(obs.game.date)) {
            val i: Int = frame.rowIx.getFirst(obs.game.date.plusDays(-1))
            if (i > 0) {
              val series: Series[Team, Double] = frame.rowAt(i)
              val mean: Double = series.mean
              val stdev: Double = series.stdev
              if (series.index.contains(obs.homeTeam) && series.index.contains(obs.awayTeam)) {
                val hv: Scalar[Double] = series.at(series.index.getFirst(obs.homeTeam))
                val av: Scalar[Double] = series.at(series.index.getFirst(obs.awayTeam))

                if (hv.isNA || av.isNA) {
                  None
                } else {
                  Some(statistic -> ((hv.get - mean) / stdev - (av.get - mean) / stdev))
                }
              } else {
                None
              }
            } else {
              None
            }
          } else {
            None
          }
      }
      val vecData: Iterable[(Statistic, Double)] = values.flatten
      if (vecData.size == stats.size) {
        Some(new DenseVector((0.0 :: vecData.map(_._2).toList).toArray))
      } else {
        None
      }
    }  else {
      None
    }
  }
}
