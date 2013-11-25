package analysis

import models.ScheduleData

trait DerivedModel extends ComputableModel {
  def baseModel: ComputableModel

  def deriveResults(result: ModelResult): ModelResult

  def computeSeason(data: List[ScheduleData]): ModelResult = {
    val modelResult = baseModel.computeSeason(data)

    deriveResults(modelResult)

  }
}
