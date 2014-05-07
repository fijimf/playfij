package analysis.predictors

import org.joda.time.LocalDate
import models.{ScheduleData, Team}
import org.apache.mahout.math.{DenseVector, Vector}
import org.saddle.scalar.Scalar
import analysis.frame.{Population, Series, Frame}

case class SingleStatFeatureMapper(name:String, frame: Frame[LocalDate, Team, Double], useZ: Boolean) extends FeatureMapper {

  override def featureDimension: Int = 2

  override def feature(obs: ScheduleData): Option[Vector] = {
    if (obs.result.isDefined) {
      if (frame.ordering.contains(obs.game.date)) {

          val series: Population[Team, Double] = frame.population(obs.game.date)
          val mean: Double = series.mean
          val stdev: Double = series.stdDev
          if (frame.ids.contains(obs.homeTeam) && frame.ids.contains(obs.awayTeam)) {
            val hv: Option[Double] = series.value(obs.homeTeam)
            val av: Option[Double] = series.value(obs.awayTeam)

            if (hv.isEmpty || av.isEmpty) {
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
  }

  override def featureName(i: Int): String = if (i==0) "Constant" else name
}
