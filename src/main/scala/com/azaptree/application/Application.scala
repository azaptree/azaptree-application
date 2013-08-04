package com.azaptree.application

import scala.annotation.tailrec
import org.slf4j.LoggerFactory
import akka.event.EventBus
import akka.event.japi.SubchannelEventBus
import Application._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import com.azaptree.application.component._
import com.azaptree.application.event._

object Application {

  /**
   * Given a list of components, returns the component dependency mappings between them : compName -> List[compDependencyName]
   */
  def componentDependencies(components: List[Component[_, _]]): Option[Map[String, List[String]]] = {
    var dependencyMap = Map[String, List[String]]()
    components.foreach { comp =>
      comp.dependsOn match {
        case Some(compDependencies) =>
          val compDependencyNames = compDependencies.map(_.name)
          dependencyMap += (comp.name -> compDependencyNames.toList)
        case None =>
      }
      val dependencies = dependencyMap.getOrElse(comp.name, Nil)
    }

    if (dependencyMap.isEmpty) None else Some(dependencyMap)
  }
}

/**
 * <ol>Components can be started and registered in 2 ways:
 * <li>Components can be started outside of the application, and then passed into the Application in the ComponentStarted state when the Application instance is constructed.
 * <li>Components can be registered with the Application, via Application.register()
 * </ol>
 *
 * Component related events are published to the eventbus, such as when the Component is started, shutdown, or shutdown failed.
 *
 * NOTE: the component startup order is controlled outside of the application.
 * However, when the application is shutdown, components will be shutdown in the proper order based on their component dependencies.
 *
 */
case class Application(components: List[Component[ComponentStarted, _]] = Nil, eventBus: SubchannelEventBus[Any, Any => Unit, Class[_]] = new AsynchronousSubchannelEventBus()) {

  val componentMap: Map[String, Component[ComponentStarted, _]] = {
    val componentMapEntries = components.map(c => (c.name, c)).toArray
    Map[String, Component[ComponentStarted, _]](componentMapEntries: _*)
  }

  def getComponentObjectClass(name: String): Option[Class[_]] = {
    componentMap.get(name) match {
      case Some(obj) => Some(obj.getClass())
      case None => None
    }
  }

  def register[A](comp: Component[ComponentNotConstructed, A]): (Application, Option[A]) = {
    assert(componentMap.get(comp.name).isEmpty, "A component with the same name is already registered: " + comp.name)

    val compStarted = comp.startup()
    val appWithNewComp = copy(components = compStarted :: components)
    eventBus.publish(ComponentStartedEvent(appWithNewComp, compStarted))
    (appWithNewComp, compStarted.componentObject)
  }

  def shutdownComponent(componentName: String, shutdownDependents: Boolean = false): Try[Application] = {
    def shutdown(app: Application, comp: Component[ComponentStarted, _]): Application = {
      try {
        val compStopped = comp.shutdown()
        val appWithCompRemoved = app.copy(components = app.components.filterNot(c => c.name == componentName))
        app.eventBus.publish(ComponentShutdownEvent(appWithCompRemoved, compStopped))
        appWithCompRemoved
      } catch {
        case e: Exception =>
          eventBus.publish(ComponentShutdownFailedEvent(this, comp, e))
          throw e
      }
    }

    Try(
      componentMap.get(componentName) match {
        case None => throw new ComponentNotFoundException(componentName)
        case Some(componentToShutdown) =>
          if (shutdownDependents) {
            componentDependencies(components) match {
              case Some(compDependencyMap) =>
                val entries = compDependencyMap.filter(_._2.contains(componentName))
                try {
                  val updatedApp = entries.keys.foldLeft(this) { (app, dependentName) =>
                    app.shutdownComponent(dependentName, true) match {
                      case Success(a) => a
                      case Failure(e) => e match {
                        case ex: ComponentNotFoundException => app
                        case _ => throw e
                      }
                    }
                  }
                  shutdown(updatedApp, componentToShutdown)
                } catch {
                  case e: Exception =>
                    eventBus.publish(ComponentShutdownFailedEvent(this, componentToShutdown, e))
                    throw e
                }
              case None => shutdown(this, componentToShutdown)
            }

          } else {
            shutdown(this, componentToShutdown)
          }
      })
  }

  def shutdown(): Application = {
    eventBus.publish(PreApplicationShutdownEvent(this))
    val log = LoggerFactory.getLogger(getClass())

    @tailrec
    def shutdownComponentsWithoutDependents(app: Application, components: Iterable[Component[ComponentStarted, _]], componentDependencies: Map[String, List[String]] = Map.empty[String, List[String]]): Application = {
      if (!components.isEmpty) {
        val dependencySet = componentDependencies.values.foldLeft(Set.empty[String])((set, compName) => set ++ compName)
        log.debug("dependencySet = {}", dependencySet)

        val findCompWithoutDependents: Component[ComponentStarted, _] => Boolean = c => !dependencySet.contains(c.name)
        val compsWithoutDependents = components.filter(findCompWithoutDependents)

        if (log.isDebugEnabled()) {
          log.debug("compsWithoutDependents = {}", compsWithoutDependents.mkString("\n", "\n\n", "\n"))
        }

        var componentDependenciesTemp = componentDependencies
        val appAfterCompShutdown = compsWithoutDependents.foldLeft(app) { (app, comp) =>
          val appAfterCompShutdown = app.shutdownComponent(comp.name) match {
            case Failure(_) => app
            case Success(application) => application
          }
          componentDependenciesTemp -= comp.name
          appAfterCompShutdown
        }

        val compsWithDependents = components.filterNot(findCompWithoutDependents)
        shutdownComponentsWithoutDependents(appAfterCompShutdown, compsWithDependents, componentDependenciesTemp)
      } else {
        return copy(components = Nil)
      }
    }

    val compDependencyMap = componentDependencies(components)

    if (log.isDebugEnabled()) {
      log.debug("componentDependencies = {}", compDependencyMap)
      log.debug("components = {}", components.mkString("\n", "\n\n", "\n"))
    }

    val app = compDependencyMap match {
      case Some(map) => shutdownComponentsWithoutDependents(this, components, map)
      case None => shutdownComponentsWithoutDependents(this, components)
    }

    eventBus.publish(PostApplicationShutdownEvent(this))
    log.debug("Published PostApplicationShutdownEvent")
    app
  }

  /**
   * Returns the order in which the components will be shutdown
   */
  def getComponentShutdownOrder(): Iterable[String] = {
    @tailrec
    def getComponentShutdownOrder(shutdownOrder: Vector[String], components: Iterable[Component[ComponentStarted, _]], componentDependencies: Map[String, List[String]]): Vector[String] = {
      if (!components.isEmpty) {
        val dependencySet = componentDependencies.values.foldLeft(Set[String]())((set, compName) => set ++ compName)

        val findCompWithoutDependents: Component[ComponentStarted, _] => Boolean = c => !dependencySet.contains(c.name)
        val compsWithoutDependents = components.filter(findCompWithoutDependents)

        var componentDependenciesTemp = componentDependencies
        val appAfterCompShutdown = compsWithoutDependents.foldLeft(shutdownOrder) { (shutdownOrder, comp) =>
          componentDependenciesTemp -= comp.name
          shutdownOrder :+ comp.name
        }

        val compsWithDependents = components.filterNot(findCompWithoutDependents)
        getComponentShutdownOrder(appAfterCompShutdown, compsWithDependents, componentDependenciesTemp)
      } else {
        return shutdownOrder
      }
    }

    componentDependencies(components) match {
      case Some(map) => getComponentShutdownOrder(Vector.empty[String], components, map)
      case None => components.map(_.name)
    }
  }

}

class ComponentNotFoundException(componentName: String) extends IllegalArgumentException(componentName)

