package analysis

import org.apache.mahout.math.{Matrix, RandomAccessSparseVector, DenseVector, SparseMatrix}
import org.apache.log4j.Logger
import models.{Team, Statistic, ScheduleData}
import org.joda.time.LocalDate
import org.apache.mahout.math.solver.LSMR

class LinearRegressionModel extends ComputableModel {
  val log = Logger.getLogger(this.getClass)

  def key = "naive-linear-regression"

  def statistics: Map[String, Statistic] = List(
    new Statistic(0, "win-predictor", "Win Predictor", 0, "Team", "%5.3f", "%5.3f", true, 101),
    new Statistic(0, "score-predictor", "Win Predictor", 0, "Team", "%5.3f", "%5.3f", true, 101)
  ).map(s => s.key -> s).toMap

  def computeSeason(data: List[ScheduleData]): ModelResult = {
    val modelResult = Map.empty[String, Map[LocalDate, Map[Long, Double]]]
    val dates: Set[LocalDate] = data.map(_.game.date).toSet
    dates.foldLeft(modelResult)((result: Map[String, Map[LocalDate, Map[Long, Double]]], date: LocalDate) => {
      val r: Map[String, Map[Long, Double]] = processDate(data.filter(!_.game.date.isAfter(date)))
      Map("win-predictor" -> (result.getOrElse("win-predictor", Map.empty[LocalDate,Map[Long, Double]]) + (date -> r("win-predictor"))),
        "score-predictor" -> (result.getOrElse("score-predictor", Map.empty[LocalDate, Map[Long, Double]]) + (date -> r("score-predictor"))))
    })
  }

  def processDate(data: List[ScheduleData]): Map[String, Map[Long, Double]] = {
    val results = data.filter(_.result.isDefined)
    val teamMap: Map[Team, Int] = (results.map(_.homeTeam) ++ results.map(_.awayTeam)).toSet.toList.sorted.zipWithIndex.toMap
    val A = results.zipWithIndex.map(pair => {
      List(
        (pair._2, teamMap(pair._1.homeTeam)) -> 1.0,
        (pair._2, teamMap(pair._1.awayTeam)) -> -1.0
      )
    }).flatten.toMap

    val b0 = results.map(g => (g.result.get.homeScore - g.result.get.awayScore).toDouble)
    val b1 = results.map(g => scala.math.signum(g.result.get.homeScore - g.result.get.awayScore).toDouble)

    val x0 = LSMRSolver.solve(A, results.size, teamMap.size, b0)
    val x1 = LSMRSolver.solve(A, results.size, teamMap.size, b1)

    Map(
      "score-predictor" -> teamMap.keySet.map(t => t.id -> x0(teamMap(t))).toMap,
      "win-predictor" -> teamMap.keySet.map(t => t.id -> x1(teamMap(t))).toMap
    )
  }

  object LSMRSolver {

    def solve(A: Map[(Int, Int), Double], aRows: Int, aCols: Int, b: List[Double]): List[Double] = {
      val Ai: SparseMatrix = createRASparseMatrix(A, aRows, aCols)
      val bi = new DenseVector(b.toArray)
      val lsmr: LSMR = new LSMR()
      lsmr.setIterationLimit(100)
      lsmr.setAtolerance(0.00001)
      lsmr.setBtolerance(0.00001)
      val xi = lsmr.solve(Ai, bi)
      0.until(aCols).map(i => xi.get(i)).toList

    }

    def createRASparseMatrix(A: Map[(Int, Int), Double], aRows: Int, aCols: Int): SparseMatrix = {
      val a: java.util.Map[java.lang.Integer, RandomAccessSparseVector] = new java.util.HashMap[java.lang.Integer, RandomAccessSparseVector]()
      A.foreach {
        case (p: (Int, Int), d: Double) => {
          if (a.containsKey(p._1)) {
            a.get(p._1).set(p._2, d)
          } else {
            val row = new RandomAccessSparseVector(aCols)
            row.set(p._2, d)
            a.put(p._1, row)
          }
        }
      }

      new SparseMatrix(aRows, aCols, a)

    }
  }

}
