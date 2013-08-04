package com.azaptree.application

import scala.concurrent.Promise
import org.slf4j.LoggerFactory
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import com.azaptree.application.healthcheck.ApplicationHealthCheck
import com.azaptree.utils._
import java.io.PrintStream
import java.util.concurrent.CountDownLatch
import com.azaptree.logging.Slf4jLogger

trait ApplicationLauncher {
  def createApplicationService(): ApplicationService
}

/**
 * Redirects stdout and stderr streams to SLF4J loggers named "stdout" and "stderr", respectively
 *
 * When the application shuts down cleanly, it performs a System.exit(0), otherwise it performs a System.exit(1)
 */
object AppLauncher extends App with Slf4jLogger {
  require(args.size > 0, "usage: scala com.azaptree.application.AppLauncher <ApplicationLauncher class name>")

  redirectStdOutErrToSl4j()

  try {
    launch()
    System.exit(0)
  } catch {
    case t: Throwable =>
      t.printStackTrace()
      System.exit(1)
  }

  def redirectStdOutErrToSl4j() = {
    val stdoutLog = LoggerFactory.getLogger("stdout")
    val stderrLog = LoggerFactory.getLogger("stderr")
    System.setOut(new PrintStream(System.out) {
      override def print(s: String) = {
        stdoutLog.info(s);
      }
    })

    System.setErr(new PrintStream(System.err) {
      override def print(s: String) = {
        stderrLog.error(s);
      }
    })
  }

  def launch() = {
    def launch(launcher: ApplicationLauncher): Unit = {
      val appService = launcher.createApplicationService()

      val shutdownLatch = new CountDownLatch(1)
      val shutdownListener: Any => Unit = event => {
        event match {
          case event: PostApplicationShutdownEvent =>
            shutdownLatch.countDown()
            log.debug("PostApplicationShutdownEvent received")
          case _ =>
            log.warn(s"Received unexpected event : $event")
        }
      }

      appService.eventBus.subscribe(shutdownListener, classOf[PostApplicationShutdownEvent])
      appService.start()
      shutdownLatch.await()
      log.debug("shutdownFuture is complete")
    }

    log.info("Application process running at : {}", PID_HOST)
    val launcherClass = args(0)
    val classLoader = getClass().getClassLoader()
    val launcher = classLoader.loadClass(launcherClass).newInstance().asInstanceOf[ApplicationLauncher]
    launch(launcher)
  }

}