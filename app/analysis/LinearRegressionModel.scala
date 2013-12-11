package analysis

import org.apache.mahout.math.{Matrix, RandomAccessSparseVector, DenseVector, SparseMatrix}
import org.apache.log4j.Logger
import models.{Statistic, ScheduleData}

class LinearRegressionModel extends ComputableModel {
    val log = Logger.getLogger(this.getClass)

    def key = "naive-linear-regression"

    def statistics: Map[String, Statistic] = List(
       new Statistic(0,"win-predictor", "Win Predictor", 0, "Team", "%5.3f", "%5.3f", true, 101),
       new Statistic(0,"score-predictor", "Win Predictor", 0, "Team", "%5.3f", "%5.3f", true, 101)
    ).map(s=>s.key->s).toMap

    def computeSeason(data: List[ScheduleData]): ModelResult = ???

//
//    def processDate(s: Schedule, date: Date, ctx: ModelContext[Team]): ModelContext[Team] = {
//      log.info("Processing " + date)
//      val games: List[Game] = s.gameList.filter(g => g.resultOpt.isDefined && !g.date.after(date))
//
//      val teamMap: Map[String, Int] = (games.map(_.homeTeam.key) ++ games.map(_.awayTeam.key)).toSet.toList.sorted.zipWithIndex.toMap
//      val A = games.zipWithIndex.map(pair => {
//        List(
//          (pair._2, teamMap(pair._1.homeTeam.key)) -> 1.0,
//          (pair._2, teamMap(pair._1.awayTeam.key)) -> -1.0
//        )
//      }).flatten.toMap
//
//      val b0 = games.map(g => (g.result.homeScore - g.result.awayScore).toDouble)
//      val b1 = games.map(g => math.signum(g.result.homeScore - g.result.awayScore).toDouble)
//
//      val x0 = LSMRSolver.solve(A,games.size, teamMap.size,  b0)
//      val x1 = LSMRSolver.solve(A,games.size, teamMap.size,  b1)
//
//
//      teamMap.foldLeft(ctx)((ctx, pair) => {
//        ctx.update(statW, date, s.teamByKey(pair._1), x0(pair._2)).
//          update(statP, date, s.teamByKey(pair._1), x1(pair._2))
//      })
//
//    }
//  }
}
