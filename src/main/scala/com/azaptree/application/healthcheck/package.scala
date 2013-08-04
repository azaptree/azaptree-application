package com.azaptree.application

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

package object healthcheck {

  /**
   * The function signature is very basic by design.
   * Any other required dependencies for the HealthCheckRunner can be "injected" in using function currying, closures, and Scala functional programming capabilities.
   *
   * The HealthCheck is run in the background, i.e., which is why a Future[HealthCheckResult] is returned
   */
  type HealthCheckRunner = (HealthCheck) => Future[HealthCheckResult]

  /**
   * Performs the actual health check and returns a health check score and an optional message.
   */
  type HealthCheckScorer = (HealthCheck) => (Int, Option[String])

  /**
   * Consists of a HealthCheck and the corresponding HealthCheckRunner which knows how to run the health check
   *
   * The HealthCheck and provides the necessary HealthCheck config
   */
  type ApplicationHealthCheck = Tuple2[HealthCheck, HealthCheckRunner]

  def healthCheckRunner(scorer: HealthCheckScorer)(implicit executionContext: ExecutionContext): HealthCheckRunner = {
    val runner: HealthCheckRunner = (healthCheck) => {
      Future[HealthCheckResult] {
        val start = System.currentTimeMillis()
        try {
          val (healthScore, info) = scorer(healthCheck)
          val end = System.currentTimeMillis()
          val healthCheckIndicator = healthCheck.computeHealthCheckIndicator(healthScore)
          HealthCheckResult(
            healthCheck = healthCheck,
            executionStart = start,
            executionEnd = end,
            healthScore = healthScore,
            healthCheckIndicator = healthCheckIndicator,
            info = info)
        } catch {
          case ex: Throwable =>
            import com.azaptree.utils._

            HealthCheckResult(
              healthCheck = healthCheck,
              executionStart = start,
              executionEnd = System.currentTimeMillis(),
              healthScore = 0,
              healthCheckIndicator = RED,
              exceptionStackTrace = Some(getExceptionStackTrace(ex)))
        }
      }
    }

    runner
  }
}