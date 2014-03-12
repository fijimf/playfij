package analysis.predictors

import org.apache.commons.math.optimization.fitting.PolynomialFitter
import org.apache.commons.math.optimization.general.GaussNewtonOptimizer
import org.apache.commons.math.analysis.polynomials.PolynomialFunction
import org.apache.commons.math.analysis.UnivariateRealFunction
import controllers.Statistics
import models.{Team, ScheduleData}
import org.saddle.Frame
import org.joda.time.LocalDate
import org.apache.commons.math.analysis.solvers.BisectionSolver

object SingleStatConditioner {
    def apply(points: List[(Double, Double, Double)]): SingleStatConditioner = {
      val ordinates: Array[Double] = points.map(_._1).toArray
      val b0: Array[Double] = points.map(_._2).toArray
      val b1: Array[Double] = points.map(_._3).toArray

      val fitterB0: PolynomialFitter = new PolynomialFitter(1, new GaussNewtonOptimizer(true))
      ordinates.zip(b0).foreach(t => fitterB0.addObservedPoint(1.0, t._1, t._2))
      val curveB0: PolynomialFunction = fitterB0.fit()

      ordinates.zip(b0).foreach(t => println("%5.1f  Fitted %5.1f    Actual %5.1f".format(t._1,curveB0.value(t._1), t._2 )))

      val fitterB1: PolynomialFitter = new PolynomialFitter(2, new GaussNewtonOptimizer(true))
      ordinates.zip(b1).foreach(t => fitterB1.addObservedPoint(1.0, t._1, t._2))
      val curveB1: PolynomialFunction = fitterB1.fit()

      ordinates.zip(b1).foreach(t => println("%5.1f  Fitted %5.1f    Actual %5.1f".format(t._1,curveB1.value(t._1), t._2 )))

      val coefficients = curveB1.getCoefficients
      val a = coefficients(2)
      val b = coefficients(1)
      val c = coefficients(0)

      val roots = ((-b + math.sqrt(b * b - 4 * a * c)) / (2 * a), (-b - math.sqrt(b * b - 4 * a * c)) / (2 * a))
      val curveFS = new UnivariateRealFunction {
        override def value(x: Double): Double = -curveB0.value(x) / curveB1.value(x)
      }

      ordinates.zip(b1).foreach(t => println("%5.1f  Z-Diff %5.1f ".format(t._1,curveFS.value(t._1))))
      println("%5.1f  Z-Diff %5.1f ".format(roots._1,curveFS.value(math.ceil(roots._1))))
      println("%5.1f  Z-Diff %5.1f ".format(roots._2,curveFS.value(math.floor(roots._2))))


      SingleStatConditioner(
        curveB0,
        curveB1,
        curveFS,
        (math.ceil(math.min(roots._1, roots._2)),math.floor(math.max(roots._1,roots._2)))
      )
    }

    def apply(scheduleData: List[ScheduleData], frame: Frame[LocalDate, Team, Double]): SingleStatConditioner = {
      val betas: List[(Double, Double, Double)] = Statistics.createSpreadBetas(scheduleData, frame).map(t => (t._1, t._2(0), t._2(1)))
      this.apply(betas)
    }
  }

  case class SingleStatConditioner(b0: UnivariateRealFunction, b1: UnivariateRealFunction, fs: UnivariateRealFunction, bracket:(Double,Double)) {
    def winProb(z: Double) = coverProb(z, 0.0)

    def coverProb(z: Double, spread: Double) = {
      val bb0: Double = b0.value(spread)
      val bb1: Double = b1.value(spread)
      val d: Double = bb0 + bb1 * z
      100 * (1.0 / (1.0 + Math.exp(-d)))
    }

    def spread(z: Double) = {
      (bracket._1).to(bracket._2,1.0).foreach(zzz=>println("%6.2f => %6.2f  %6.3f".format(zzz, fs.value(zzz), 1.0/(1+math.exp(-(b0.value(0)+b1.value(0)*fs.value(zzz)))))))
      val solver: BisectionSolver = new BisectionSolver()
      val function = new UnivariateRealFunction() {
        def value(x: Double): Double = {
          fs.value(x) - z
        }
      }
      val solve: Double = solver.solve(100, function, bracket._1, bracket._2, 0.0)

      println(z+"  "+solve)
      solve
    }

}
