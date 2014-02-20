package analysis.predictors

import org.apache.mahout.classifier.sgd.{L1, OnlineLogisticRegression}
import org.apache.mahout.math.{Matrix, DenseVector, Vector}
import models.{Team, ScheduleData}
import org.saddle.{Series, Frame}
import org.joda.time.LocalDate
import org.saddle.scalar.Scalar

trait FeatureMapper {
  def featureDimension:Int
  def feature(obs:ScheduleData):Option[Vector]
}

trait Categorizer {
  def category(d:ScheduleData):Option[Int]
}


object Predictor {
  def regress(data: List[ScheduleData], fm: FeatureMapper, cat:Categorizer): List[Double] = {
    val logisticRegression: OnlineLogisticRegression = new OnlineLogisticRegression(2, fm.featureDimension, new L1()).lambda(0.0).learningRate(1)
    val trainingSet: List[(Vector, Int)] = for (d <- data; feat <- fm.feature(d); c <- cat.category(d)) yield feat -> c

    1.to(25).foreach(i => {
      trainingSet.foreach(obs => {
        logisticRegression.train(obs._2, obs._1)
      })
    })
    val beta: Matrix = logisticRegression.getBeta
    0.until(fm.featureDimension).map(i => beta.get(0, i)).toList
  }

  def spreadCategorizer(x: Double = 0) = {
    new Categorizer {
      override def category(d: ScheduleData): Option[Int] = d.result.map(r => if ((r.homeScore - r.awayScore) > x) 1 else 0)
    }
  }

  def statFeatureMapper(frame: Frame[LocalDate, Team, Double], useZ:Boolean)= new FeatureMapper(){
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
                    Some(new DenseVector(Array(1.0,((hv.get - mean) / stdev) - ((av.get - mean) / stdev))))
                  } else {
                    Some(new DenseVector(Array(1.0,hv.get - av.get)))
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
  }
  
}
