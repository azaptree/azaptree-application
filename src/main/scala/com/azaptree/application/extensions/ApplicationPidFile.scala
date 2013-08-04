package com.azaptree.application.extensions

import com.azaptree.application.ApplicationExtension
import java.io.File
import com.azaptree.nio.file.FileWatcherService
import com.azaptree.utils._
import org.apache.commons.io.FileUtils
import java.nio.file.StandardWatchEventKinds._
import com.azaptree.application.ApplicationService
import org.slf4j.LoggerFactory
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds._

object ApplicationPidFile {

  def apply(appName: String, pidDir: File)(implicit fileWatcherService: FileWatcherService, applicationService: ApplicationService) = {
    new ApplicationPidFile(appName, pidDir)
  }
}

/**
 * Creates a PID file in the specified dir. When the pid file is deleted it will stop the ApplicationService.
 */
class ApplicationPidFile(appName: String, pidDir: File)(implicit fileWatcherService: FileWatcherService, applicationService: ApplicationService)
    extends ApplicationExtension {
  require(!appName.trim().isEmpty(), "appName cannot be blank")

  /**
   * Creates a PID file using the following naming pattern: $appName_$HOST_$PID.pid
   */
  def pidFile = new File(pidDir, s"${appName}_${HOST}_$PID.pid")

  override def start() = {
    if (!pidDir.exists()) {
      pidDir.mkdirs()
    }

    if (!pidDir.exists()) {
      throw new IllegalStateException(s"Unable to create watchDir: pidDir")
    }

    val log = LoggerFactory.getLogger("ApplicationPidFile")
    val f = pidFile
    FileUtils.touch(f)
    log.info("Created  PID file : {}", f.getAbsolutePath())
    fileWatcherService.watch(pidDir.toPath(), ENTRY_DELETE :: Nil, (watchEvent) => {
      watchEvent.context() match {
        case p: Path =>
          log.info("Received WatchEvent for : %s -> %s".format(p, watchEvent.kind().name()))
          if (p.getFileName().toString() == f.getName() && watchEvent.kind() == ENTRY_DELETE) {
            log.info("PID file has been deleted, which is a trigger to stop the application : {}", f)
            applicationService.stop()
          }
        case _ =>
      }

    })

    pidFile.deleteOnExit()
  }

  override def stop() = { /*no action needed*/ }
}