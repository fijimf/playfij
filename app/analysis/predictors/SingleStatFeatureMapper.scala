package analysis.predictors

import org.saddle.{Series, Frame}
import org.joda.time.LocalDate
import models.{ScheduleData, Team}
import org.apache.mahout.math.{DenseVector, Vector}
import org.saddle.scalar.Scalar

case class SingleStatFeatureMapper(name:String, frame: Frame[LocalDate, Team, Double], useZ: Boolean) extends FeatureMapper {

  override def featureDimension: Int = 2

  override def feature(obs: ScheduleData): Option[Vector] = {
    if (obs.result.isDefined) {
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

              if (useZ) {
                Some(new DenseVector(Array(1.0, ((hv.get - mean) / stdev) - ((av.get - mean) / stdev))))
              } else {
                Some(new DenseVector(Array(1.0, hv.get - av.get)))
              }
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
    } else {
      None
    }
  }

  override def featureName(i: Int): String = if (i==0) "Constant" else name
}
