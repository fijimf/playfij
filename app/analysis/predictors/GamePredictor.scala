package analysis.predictors

import models.{Team, ScheduleData}

trait WinnerPredictor {
  def winner(s: ScheduleData): Option[Team]
}

trait SpreadPredictor extends WinnerPredictor {
  def spread(s: ScheduleData): Option[Double]

  def winner(s: ScheduleData) = {
    spread(s) match {
      case Some(x) if x > 0 => Some(s.homeTeam)
      case Some(x) if x < 0 => Some(s.awayTeam)
      case _ => None
    }
  }
}

trait OverUnderPredictor {
  def overUnder(s: ScheduleData): Option[Double]
}

trait ScorePredictor extends SpreadPredictor with OverUnderPredictor {
  def scores(s: ScheduleData): Option[(Double, Double)]

  def spread(s: ScheduleData) = {
    scores(s).map(t => t._1 - t._2)
  }

  def overUnder(s: ScheduleData) = {
    scores(s).map(t => t._1 + t._2)
  }
}

trait ProbabilityPredictor {
  def probabilityHome(s: ScheduleData): Option[Double]

  def probabilityAway(s: ScheduleData) = probabilityHome(s).map(1 - _)
}


