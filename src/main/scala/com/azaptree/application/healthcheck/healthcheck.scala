package com.azaptree.application.healthcheck

import com.azaptree.concurrent.TaskSchedule
import com.typesafe.config.Config

case class HealthCheck(config: HealthCheckConfig)

case class HealthCheckConfig(
    group: String = "",
    name: String,
    description: Option[String] = None,
    enabled: Boolean = true,
    /** Used to indicate the importance of a healthcheck relative to other healthchecks */
    importanceLevel: Short = 0,
    greenRange: GreenHeathCheckIndicatorThreshold = GreenHeathCheckIndicatorThreshold(90),
    yellowRange: YellowHeathCheckIndicatorThreshold = YellowHeathCheckIndicatorThreshold(75),
    redRange: RedHeathCheckIndicatorThreshold = RedHeathCheckIndicatorThreshold(0),
    config: Option[Config] = None,
    schedule: Option[TaskSchedule] = None)(displayName: String = s"$group:$name") {

  def computeHealthCheckIndicator(healthScore: Int): HealthCheckIndicator = {
    if (healthScore >= greenRange.minScore) {
      GREEN
    } else if (healthScore >= yellowRange.minScore) {
      YELLOW
    } else {
      RED
    }
  }

}

sealed trait HeathCheckIndicatorThreshold {
  val indicator: HealthCheckIndicator
  val minScore: Int
}

case class GreenHeathCheckIndicatorThreshold(minScore: Int) extends HeathCheckIndicatorThreshold {
  val indicator = GREEN
}

case class YellowHeathCheckIndicatorThreshold(minScore: Int) extends HeathCheckIndicatorThreshold {
  val indicator = YELLOW
}

case class RedHeathCheckIndicatorThreshold(minScore: Int) extends HeathCheckIndicatorThreshold {
  val indicator = RED
}

sealed trait HealthCheckIndicator

case object GREEN extends HealthCheckIndicator
case object YELLOW extends HealthCheckIndicator
case object RED extends HealthCheckIndicator

case class HealthCheckResult(
  healthCheck: HealthCheck,
  /** when the healthcheck execution started in Epoch time */
  executionStart: Long,
  /** when the healthcheck execution ended in Epoch time */
  executionEnd: Long,
  healthScore: Int,
  healthCheckIndicator: HealthCheckIndicator,
  info: Option[String] = None,
  exceptionStackTrace: Option[String] = None)
