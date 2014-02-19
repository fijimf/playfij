package analysis.predictors

import org.apache.mahout.classifier.sgd.{L1, PriorFunction, OnlineLogisticRegression}
import org.apache.mahout.math.{Matrix, DenseVector, ConstantVector, Vector}
import scala.util.Random
import models.ScheduleData

trait FeatureMapper {
  def featureDimension:Int
  def feature(obs:ScheduleData):Vector
  def category(obs:ScheduleData):Int

}


object Predictor {
  def regress(data: List[ScheduleData], fm: FeatureMapper): List[Double] = {
    val logisticRegression: OnlineLogisticRegression = new OnlineLogisticRegression(2, fm.featureDimension, new L1()).lambda(0.0).learningRate(1)
    val trainingSet: IndexedSeq[(Vector, Int)] = data.map(d => {
      fm.feature(d) -> fm.category(d)
    }).toIndexedSeq

    1.to(25).foreach(i => {
      trainingSet.foreach(obs => {
        logisticRegression.train(obs._2, obs._1)
      })
    })
    val accuracy = trainingSet.map(obs => {
      val d = if (logisticRegression.classify(obs._1).get(0) > 0.5) 1 else 0
      if (d == obs._2) 1 else 0
    }).sum / (1.0 * trainingSet.size)
    val beta: Matrix = logisticRegression.getBeta
    0.to(fm.featureDimension).map(i => beta.get(0, i)).toList
  }

  def evaluate(data: List[(Double, Double, Double, Double, Double)]): List[(Double, ((Double, Double), (Double, Double)))] = {
    List(-12.5, -10.5, -8.5, -6.5, -4.5, -2.5, 0.0, 2.5, 4.5, 6.5, 8.5, 10.5, 12.5).map(x => {
      val v: ((Double, Double, Double, Double, Double)) => Double = t => t._3 - t._4
      val z: ((Double, Double, Double, Double, Double)) => Double = t => t._1 - t._2
      x ->(getBeta(data, z, x), getBeta(data, v, x))
    })
  }

  def getBeta(data: List[(Double, Double, Double, Double, Double)], fn: ((Double, Double, Double, Double, Double)) => Double, x: Double): (Double, Double) = {
    val logisticRegression: OnlineLogisticRegression = new OnlineLogisticRegression(2, 2, new L1()).lambda(0.0).learningRate(1)
    val trainingSet: IndexedSeq[(DenseVector, Int)] = data.map(tup => {
      val featureValue: Double = fn(tup)
      val category: Int = if (tup._5 > x) 1 else 0
      IndexedSeq(new DenseVector(Array(1.0, featureValue)) -> category)
    }).toIndexedSeq.flatten

    println("Covers %d/%d".format(trainingSet.map(_._2).sum, trainingSet.size))
    1.to(25).foreach(i => {
      trainingSet.foreach(obs => {
        logisticRegression.train(obs._2, obs._1)
      })
    })
    val accuracy = trainingSet.map(obs => {
      // println("%5.3f %d".format(logisticRegression.classify(obs._1).get(0), obs._2))
      val d = if (logisticRegression.classify(obs._1).get(0) > 0.5) 1 else 0
      if (d == obs._2) 1 else 0
    }).sum / (1.0 * trainingSet.size)
    println("Accuracy " + accuracy)
    println(logisticRegression.getBeta)
    (logisticRegression.getBeta.get(0, 0), accuracy)

  }
}
