package com.azaptree.application

import scala.language.existentials
import com.azaptree.application.component._

trait ApplicationEvent

case class ComponentStartedEvent(app: Application, comp: Component[ComponentStarted, _]) extends ApplicationEvent

case class ComponentShutdownEvent(app: Application, comp: Component[ComponentStopped, _]) extends ApplicationEvent

case class ComponentShutdownFailedEvent(app: Application, comp: Component[ComponentStarted, _], exception: Exception) extends ApplicationEvent

case class PreApplicationShutdownEvent(app: Application) extends ApplicationEvent

case class PostApplicationShutdownEvent(app: Application) extends ApplicationEvent