package com.azaptree.application.componentLifeCycles.nio

import com.azaptree.application.component._
import com.azaptree.application.component.ComponentState._
import com.azaptree.nio.file.FileWatcherService

class FileWatcherServiceComponentLifeCycle extends ComponentLifeCycle[FileWatcherService] {

  override def create(comp: Component[ComponentNotConstructed, FileWatcherService]): Component[ComponentConstructed, FileWatcherService] = {
    val service = new FileWatcherService {}
    comp.copy[ComponentConstructed, FileWatcherService](componentObject = Some(service))
  }

  override def stop(comp: Component[ComponentStarted, FileWatcherService]): Component[ComponentStopped, FileWatcherService] = {
    comp.componentObject.foreach(_.destroy())
    comp.copy[ComponentStopped, FileWatcherService](componentObject = None)
  }

}