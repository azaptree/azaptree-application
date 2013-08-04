package test.com.azaptree.application

import scala.concurrent.ExecutionContext.Implicits.global
import com.azaptree.application.ApplicationLauncher
import com.azaptree.application.ApplicationService
import com.azaptree.application.component._
import com.azaptree.application.healthcheck._
import com.azaptree.application.healthcheck.HealthCheck
import com.azaptree.application.healthcheck._
import com.typesafe.config.ConfigFactory
import TestApplicationLauncher._
import com.azaptree.nio.file.FileWatcherService
import com.azaptree.application.pidFile.ApplicationPidFile
import com.azaptree.application.ApplicationExtension
import com.azaptree.application.ApplicationExtensionComponentLifeCycle
import java.io.File
import com.azaptree.application.componentLifeCycles.nio.FileWatcherServiceComponentLifeCycle

object TestApplicationLauncher {
  val started = "ComponentStarted"
  val initialized = "ComponentInitialized"
  val constructed = "ComponentConstructed"

  var reverseShutdownOrder: List[String] = Nil

  case class Comp(state: List[String] = Nil) {
    def addState(newState: String): Comp = {
      copy(state = newState :: state)
    }
  }

  class CompLifeCycle extends ComponentLifeCycle[Comp] {
    protected def create(comp: Component[ComponentNotConstructed, Comp]): Component[ComponentConstructed, Comp] = {
      comp.copy[ComponentConstructed, Comp](componentLifeCycle = this, componentObject = Some(Comp(constructed :: Nil)))
    }

    override protected def init(comp: Component[ComponentConstructed, Comp]): Component[ComponentInitialized, Comp] = {
      val Comp = comp.componentObject.get
      comp.copy[ComponentInitialized, Comp](componentObject = Some(Comp.addState(initialized)))
    }

    override protected def start(comp: Component[ComponentInitialized, Comp]): Component[ComponentStarted, Comp] = {
      val Comp = comp.componentObject.get
      val compStarted = comp.copy[ComponentStarted, Comp](componentObject = Some(Comp.addState(started)))
      compStarted
    }

    override protected def stop(comp: Component[ComponentStarted, Comp]): Component[ComponentStopped, Comp] = {
      val compStopped = comp.copy[ComponentStopped, Comp](componentObject = None)
      reverseShutdownOrder = comp.name :: reverseShutdownOrder
      compStopped
    }

  }

  class CompLifeCycleWithShutdownFailure extends ComponentLifeCycle[Comp] {
    protected def create(comp: Component[ComponentNotConstructed, Comp]): Component[ComponentConstructed, Comp] = {
      comp.copy[ComponentConstructed, Comp](componentLifeCycle = this, componentObject = Some(Comp(constructed :: Nil)))
    }

    override protected def init(comp: Component[ComponentConstructed, Comp]): Component[ComponentInitialized, Comp] = {
      val Comp = comp.componentObject.get
      comp.copy[ComponentInitialized, Comp](componentObject = Some(Comp.addState(initialized)))
    }

    override protected def start(comp: Component[ComponentInitialized, Comp]): Component[ComponentStarted, Comp] = {
      val Comp = comp.componentObject.get
      val compStarted = comp.copy[ComponentStarted, Comp](componentObject = Some(Comp.addState(started)))
      compStarted
    }

    override protected def stop(comp: Component[ComponentStarted, Comp]): Component[ComponentStopped, Comp] = {
      throw new RuntimeException("SHUTDOWN ERROR")
    }
  }

  sealed trait Event

  sealed trait Event2 extends Event

  case object EventA extends Event

  case object EventB extends Event

  case object EventC extends Event2
}

class TestApplicationLauncher extends ApplicationLauncher {

  val compAHealthCheck = HealthCheck(
    group = "GROUP-1", name = "compAHealthCheck",
    config = Some(ConfigFactory.parseString("""compName = "CompA" """)))("CompA HealthCheck")
  val compCHealthCheck = HealthCheck(
    group = "GROUP-10", name = "compCHealthCheck",
    config = Some(ConfigFactory.parseString("""compName = "CompC" """)))("CompC HealthCheck")
  val compEHealthCheck = HealthCheck(
    name = "compEHealthCheck",
    config = Some(ConfigFactory.parseString("""compName = "CompE" """)))("CompE HealthCheck")

  val greenCheckScorer: HealthCheckScorer = (healthCheck) => (100, None)
  val yellowCheckScorer: HealthCheckScorer = (healthCheck) => (80, None)
  val redCheckScorer: HealthCheckScorer = (healthCheck) => (50, Some("ALERT!!!"))

  import scala.concurrent.ExecutionContext.Implicits.global

  val compA = Component[ComponentNotConstructed, Comp](name = "CompA", componentLifeCycle = new CompLifeCycle(),
    healthChecks = Some((compAHealthCheck, healthCheckRunner(greenCheckScorer)) :: Nil))
  val compB = Component[ComponentNotConstructed, Comp](name = "CompB", componentLifeCycle = new CompLifeCycle())
  val compC = Component[ComponentNotConstructed, Comp](name = "CompC", componentLifeCycle = new CompLifeCycle(),
    healthChecks = Some((compCHealthCheck, healthCheckRunner(yellowCheckScorer)) :: Nil))
  val compD = Component[ComponentNotConstructed, Comp](name = "CompD", componentLifeCycle = new CompLifeCycle())
  val compE = Component[ComponentNotConstructed, Comp](name = "CompE", componentLifeCycle = new CompLifeCycle(),
    healthChecks = Some((compEHealthCheck, healthCheckRunner(redCheckScorer)) :: Nil))

  var comps = Vector[Component[ComponentNotConstructed, Comp]]()
  comps = comps :+ compA.copy[ComponentNotConstructed, Comp](dependsOn = Some((compB :: compD :: Nil)))
  comps = comps :+ compB.copy[ComponentNotConstructed, Comp](dependsOn = Some((compC :: Nil)))
  comps = comps :+ compC
  comps = comps :+ compD.copy[ComponentNotConstructed, Comp](dependsOn = Some((compB :: Nil)))
  comps = comps :+ compE.copy[ComponentNotConstructed, Comp](dependsOn = Some((compD :: Nil)))

  val appHealthCheck1 = HealthCheck(
    group = "GROUP-1", name = "appHealthCheck1",
    config = Some(ConfigFactory.parseString("""compName = "CompA" """)))("appHealthCheck1")

  val appHealthCheck2 = HealthCheck(
    group = "GROUP-1", name = "appHealthCheck2",
    config = Some(ConfigFactory.parseString("""compName = "CompB" """)))("appHealthCheck2")

  val appHealthCheck3 = HealthCheck(
    group = "GROUP-2", name = "appHealthCheck3",
    config = Some(ConfigFactory.parseString("""compName = "CompC" """)))("appHealthCheck3")

  val checkCompStartedScorer: (ApplicationService, HealthCheck) => (Int, Option[String]) = (appService, healthCheck) => {
    val compName = healthCheck.config.get.getString("compName")
    if (appService.isComponentStarted(compName)) (100, None) else (0, Some(s"$compName is not started"))
  }

  override def createApplicationService(): ApplicationService = {
    implicit val app = new ApplicationService()
    comps.foreach(app.registerComponent(_))

    val fileWatcherComponent = Component[ComponentNotConstructed, FileWatcherService]("FileWatcherService", new FileWatcherServiceComponentLifeCycle())
    implicit val fileWatcherService = app.registerComponent(fileWatcherComponent).get

    val appPidFile = ApplicationPidFile("ApplicationPidFile", new File("target/TestApplicationLauncher"))
    app.registerComponent(Component[ComponentNotConstructed, ApplicationExtension]("ApplicationPidFile", new ApplicationExtensionComponentLifeCycle(appPidFile)))

    val scorer = checkCompStartedScorer.curried(app)
    app.addHealthCheck(appHealthCheck1, healthCheckRunner(scorer))
    app.addHealthCheck(appHealthCheck2, healthCheckRunner(scorer))
    app.addHealthCheck(appHealthCheck3, healthCheckRunner(scorer))

    app
  }
}

