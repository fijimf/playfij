package analysis.predictors

import org.apache.mahout.classifier.sgd.{L1, OnlineLogisticRegression}
import org.apache.mahout.math.{Matrix, DenseVector, Vector}
import models.{Team, ScheduleData}
import org.saddle.{Series, Frame}
import org.joda.time.LocalDate
import org.saddle.scalar.Scalar

object Predictor {
  def regress(data: List[ScheduleData], fm: FeatureMapper, cat:Categorizer, numPasses:Int=25): List[Double] = {
    val logisticRegression: OnlineLogisticRegression = new OnlineLogisticRegression(2, fm.featureDimension, new L1()).lambda(0.0).learningRate(1)
    val trainingSet: List[(Vector, Int)] = for (d <- data; feat <- fm.feature(d); c <- cat.category(d)) yield feat -> c

    1.to(numPasses).foreach(i => {
      trainingSet.foreach(obs => {
        logisticRegression.train(obs._2, obs._1)
      })
    })
    val beta: Matrix = logisticRegression.getBeta
    0.until(fm.featureDimension).map(i => beta.get(0, i)).toList
  }


}
