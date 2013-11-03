package models

case class Statistic(
                      id: Long,
                      key: String,
                      name: String,
                      modelId: Long,
                      targetDomain: String,
                      shortFormat: String,
                      longFormat: String,
                      higherIsBetter: Boolean
                      )