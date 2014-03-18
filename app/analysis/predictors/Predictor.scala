package analysis.predictors

import org.apache.mahout.classifier.sgd.{L1, OnlineLogisticRegression}
import org.apache.mahout.math.{Matrix, DenseVector, Vector}
import models.{Team, ScheduleData}
import org.saddle.{Series, Frame}
import org.joda.time.LocalDate
import org.saddle.scalar.Scalar
import play.api.Logger

object Predictor {
  val logger:Logger = Logger("Predictor")
  def regress(data: List[ScheduleData], fm: FeatureMapper, cat:Categorizer, numPasses:Int=25): List[Double] = {
    val logisticRegression: OnlineLogisticRegression = new OnlineLogisticRegression(2, fm.featureDimension, new L1()).lambda(0.0).learningRate(1)
    val trainingSet: List[(Vector, Int)] = for (d <- data; feat <- fm.feature(d); c <- cat.category(d)) yield feat -> c

    1.to(numPasses).foreach(i => {
      trainingSet.foreach(obs => {
        logisticRegression.train(obs._2, obs._1)
      })
    })
    val beta: Matrix = logisticRegression.getBeta
    0.until(fm.featureDimension).map{i =>
      val b: Double = beta.get(0, i)
      logger.info("%-20s %-20s  %7.4f".format(cat.name, fm.featureName(i), b))
      b
    }.toList
  }


}
