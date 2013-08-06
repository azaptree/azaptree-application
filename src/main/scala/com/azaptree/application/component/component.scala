package com.azaptree.application.component

import com.azaptree.logging.Slf4jLogger
import com.azaptree.application.healthcheck._
import reflect.runtime.universe._
import scala.util.control.Breaks._
import ComponentState._

/**
 * Every Component has a unique name. The Component transitions between ComponentStates via its ComponentLifeCycle :
 *
 * ComponentNotConstructed -> ComponentConstructed -> ComponentInitialized -> ComponentStarted -> ComponentStopped
 *
 * ComponentStopped represents the end of life for the Component.
 * In order to start the Component again, it needs to start over from the ComponentNotConstructed state via its ComponentLifeCycle.
 *
 * A Component lists its dependencies, which can be used by the Application to manage the proper start up and shutdown component sequence.
 *
 * A Component also lists its health checks.
 */
case class Component[S <: ComponentState, A](
    name: String,
    componentLifeCycle: ComponentLifeCycle[A],
    componentObject: Option[A] = None,
    dependsOn: Option[Iterable[Component[_, _]]] = None,
    healthChecks: Option[List[ApplicationHealthCheck]] = None) {

  def startup(): Component[ComponentStarted, A] = {
    assert(componentObject.isEmpty, "It is invalid to startup a Component that is not in the NotConstructed state or already has some component object")

    componentLifeCycle.startUp(copy[ComponentNotConstructed, A]())
  }

  def shutdown(): Component[ComponentStopped, A] = {
    componentObject.foreach(o => {
      componentLifeCycle.shutdown(copy[ComponentStarted, A]())
    })

    copy[ComponentStopped, A](componentObject = None)
  }

}

sealed trait ComponentState

object ComponentState {
  sealed class ComponentNotConstructed extends ComponentState
  sealed class ComponentConstructed extends ComponentState
  sealed class ComponentInitialized extends ComponentState
  sealed class ComponentStarted extends ComponentState
  sealed class ComponentStopped extends ComponentState
}

/**
 * Knows how to manage the lifecycle for a Component
 */
trait ComponentLifeCycle[A] extends Slf4jLogger {

  /**
   * The Component[ComponentConstructed, A] that is returned will have componentObject = Some(A)
   */
  protected def create(comp: Component[ComponentNotConstructed, A]): Component[ComponentConstructed, A]

  protected def init(comp: Component[ComponentConstructed, A]): Component[ComponentInitialized, A] = comp.copy[ComponentInitialized, A]()

  protected def start(comp: Component[ComponentInitialized, A]): Component[ComponentStarted, A] = comp.copy[ComponentStarted, A]()

  protected def stop(comp: Component[ComponentStarted, A]): Component[ComponentStopped, A] = comp.copy[ComponentStopped, A](componentObject = None)

  /**
   * This will startup a new instance of the component
   *
   * The Component[ComponentStarted, A] that is returned will have componentObject = Some(A)
   */
  final def startUp(comp: Component[ComponentNotConstructed, A]): Component[ComponentStarted, A] = {

    val constructed = comp.componentLifeCycle.create(comp)
    log.debug("ComponentConstructed : {}", comp.name)

    val initialized = constructed.componentLifeCycle.init(constructed)
    log.debug("ComponentInitialized : {}", comp.name)

    val started = initialized.componentLifeCycle.start(initialized)
    log.info("ComponentStarted : {}", comp.name)

    started
  }

  /**
   * The Component[ComponentStopped, A] that is returned will have componentObject = None
   */
  final def shutdown(comp: Component[ComponentStarted, A]): Component[ComponentStopped, A] = {
    val stopped = comp.componentLifeCycle.stop(comp)
    log.info("ComponentStopped : {}", comp.name)
    stopped
  }

}