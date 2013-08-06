package com.azaptree.application

import com.azaptree.application.component._
import com.azaptree.application.component.ComponentState._

trait ApplicationExtension {

  def start(): Unit

  def stop(): Unit
}

class ApplicationExtensionComponentLifeCycle(extension: ApplicationExtension) extends ComponentLifeCycle[ApplicationExtension] {

  override def create(comp: Component[ComponentNotConstructed, ApplicationExtension]): Component[ComponentConstructed, ApplicationExtension] = {
    comp.copy[ComponentConstructed, ApplicationExtension](componentObject = Some(extension))
  }

  override def start(comp: Component[ComponentInitialized, ApplicationExtension]): Component[ComponentStarted, ApplicationExtension] = {
    comp.componentObject.foreach(_.start())
    comp.copy[ComponentStarted, ApplicationExtension]()
  }

  override def stop(comp: Component[ComponentStarted, ApplicationExtension]): Component[ComponentStopped, ApplicationExtension] = {
    comp.componentObject.foreach(_.stop())
    comp.copy[ComponentStopped, ApplicationExtension]()
  }

}