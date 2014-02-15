package analysis.predictors

import org.apache.mahout.classifier.sgd.{L1, PriorFunction, OnlineLogisticRegression}
import org.apache.mahout.math.{DenseVector, ConstantVector, Vector}
import scala.util.Random

object Predictor {
  def evaluate(data: List[(Double, Double, Double, Double, Double)]): List[(Int, ((Double, Double), (Double, Double)))] = {
    List(-12, -10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12).map(x => {
      val v: ((Double, Double, Double, Double, Double)) => Double = t => t._3 - t._4
      val z: ((Double, Double, Double, Double, Double)) => Double = t => t._1 - t._2
      x ->(getBeta(data, z, x), getBeta(data, v, x))
    })
  }

  def getBeta(data: List[(Double, Double, Double, Double, Double)], fn: ((Double, Double, Double, Double, Double)) => Double, x: Int): (Double,Double) = {
    val logisticRegression: OnlineLogisticRegression = new OnlineLogisticRegression(2, 1, new L1()).lambda(0.1).learningRate(1)
    val trainingSet: IndexedSeq[(DenseVector, Int)] = data.map(tup => new DenseVector(Array(fn(tup))) -> (if (tup._5 > x) 1 else 0)).toIndexedSeq
    1.to(20).foreach(i => {
      trainingSet.foreach(obs => {
        logisticRegression.train(obs._2, obs._1)
      })
    })
    val accuracy = trainingSet.map(obs=>{
      println("%5.3f %d".format(logisticRegression.classify(obs._1).get(0), obs._2))
       val d = if (logisticRegression.classify(obs._1).get(0) > 0.5) 1 else 0
       if (d==obs._2) 1 else 0
    }).sum/(1.0*trainingSet.size)
    println("Accuracy "+accuracy)
    ( logisticRegression.getBeta.get(0, 0), accuracy)

  }
}
