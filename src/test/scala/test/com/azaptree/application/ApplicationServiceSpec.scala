package test.com.azaptree.application

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.azaptree.application.ApplicationService
import com.azaptree.application.component._
import com.azaptree.application.healthcheck._
import com.typesafe.config.ConfigFactory
import ApplicationServiceSpec._
import ApplicationServiceSpec._
import scala.concurrent.Future
import scala.concurrent.Await
import com.azaptree.application.InvalidComponentNameException
import com.azaptree.concurrent.PeriodicTaskSchedule
import org.slf4j.LoggerFactory
import com.azaptree.application.ApplicationService

object ApplicationServiceSpec {
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

class ApplicationServiceSpec extends FunSpec with ShouldMatchers {
  val log = LoggerFactory.getLogger("ApplicationServiceSpec")

  val compAHealthCheck = HealthCheck(
    group = "GROUP-1", name = "compAHealthCheck",
    schedule = Some(PeriodicTaskSchedule(period = 1)),
    config = Some(ConfigFactory.parseString("""compName = "CompA" """)))("CompA HealthCheck")
  val compCHealthCheck = HealthCheck(
    group = "GROUP-10", name = "compCHealthCheck",
    config = Some(ConfigFactory.parseString("""compName = "CompC" """)))("CompC HealthCheck")
  val compEHealthCheck = HealthCheck(
    name = "compEHealthCheck",
    config = Some(ConfigFactory.parseString("""compName = "CompE" """)))("CompE HealthCheck")

  var greenCheckScorerRunCount = 0

  val greenCheckScorer: HealthCheckScorer = (healthCheck) => {
    greenCheckScorerRunCount += 1
    log.info("greenCheckScorerRunCount = {}", greenCheckScorerRunCount)
    (100, None)
  }
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

  def createApp() = {
    log.info(comps.mkString("\n\n***************** comps *****************\n", "\n\n", "\n*************************************\n"))

    val compCreator = comps.toList
    val app = new ApplicationService()

    comps.foreach(app.registerComponent(_))

    app.addHealthCheck(appHealthCheck1, healthCheckRunner(checkCompStartedScorer.curried(app)))
    app.addHealthCheck(appHealthCheck2, healthCheckRunner(checkCompStartedScorer.curried(app)))
    app.addHealthCheck(appHealthCheck3, healthCheckRunner(checkCompStartedScorer.curried(app)))

    app
  }

  describe("An ApplicationService") {
    it("is used to start and stop components defined for an Application") {
      val app = createApp()
      try {
        app.start()
        log.info("*** app components = " + app.componentNames.mkString("\n\n", "\n", "\n\n"))

        app.stop()

        val shutdownOrder = reverseShutdownOrder.reverse
        log.info("*** shutdownOrder = " + shutdownOrder)

        shutdownOrder.indexOf("CompA") should be < 2
        shutdownOrder.indexOf("CompE") should be < 2
        shutdownOrder.indexOf("CompD") should be(2)
        shutdownOrder.indexOf("CompB") should be(3)
        shutdownOrder.indexOf("CompC") should be(4)

        shutdownOrder match {
          case ("CompE" :: "CompA" :: "CompD" :: "CompB" :: "CompC" :: Nil) =>
          case ("CompA" :: "CompE" :: "CompD" :: "CompB" :: "CompC" :: Nil) =>
          case _ => fail("Shutdown order is not correct")
        }
      } finally {
        app.stop()
      }
    }

    it("can return the names of components that have been registered with the ApplicationService") {
      val app = createApp()

      try {
        app.componentNames.size should be(comps.size)
        comps.foreach(c => assert(app.componentNames.find(name => name == c.name).isDefined, {
          val compName = c.name
          val compNames = app.componentNames
          s"app does does not contain [$compName] within : $compNames"
        }))
      } finally {
        app.stop()
      }

    }

    it("is used used to register components on an individual basis") {
      val app = createApp()

      try {
        val compF = Component[ComponentNotConstructed, Comp]("CompF", new CompLifeCycle())
        app.registerComponent(compF)
        app.componentNames should contain(compF.name)
        app.componentNames.size should be(comps.size + 1)
      } finally {
        app.stop()
      }
    }

    it("will start up all components that have been supplied at ApplicationService construction time") {
      val app = createApp()
      try {
        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)

        app.start()

        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)
        app.startedComponentNames.size should be(app.componentNames.size)

        app.stop()
        app.isRunning() should be(false)
        app.startedComponentNames.isEmpty should be(true)
      } finally {
        app.stop()
      }
    }

    it("can publish ApplicationEvents") {
      val app = createApp()
      try {
        var count = 0;
        app.eventBus.subscribe((event) => {
          count += 1
        }, classOf[String])

        app.eventBus.publish("TEST MESSAGE");

        val start = System.currentTimeMillis()
        while (count != 1) {
          if (System.currentTimeMillis() - start > 1000l) {
            count should be(1)
          }
          Thread.`yield`
        }

      } finally {
        app.stop()
      }
    }

    it("implements a subchannel event bus") {
      val app = createApp()
      try {
        var eventCount = 0;
        app.eventBus.subscribe((event) => {
          eventCount += 1
        }, classOf[Event])

        var event2Count = 0;
        app.eventBus.subscribe((event) => {
          event2Count += 1
        }, classOf[Event2])

        app.eventBus.publish(EventA);
        app.eventBus.publish(EventB);
        app.eventBus.publish(EventC);

        val start = System.currentTimeMillis()
        while (eventCount != 3 && event2Count != 1) {
          if (System.currentTimeMillis() - start > 1000l) {
            eventCount should be(3)
            event2Count should be(1)
          }
          Thread.`yield`
        }

      } finally {
        app.stop()
      }
    }

    it("can start and stop registered components on an individual basis") {
      val app = createApp()
      try {
        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)

        app.start()

        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)
        app.startedComponentNames.size should be(app.componentNames.size)

        app.startedComponentNames.foreach(app.stopComponent(_))

        app.isRunning() should be(false)
        app.startedComponentNames.isEmpty should be(true)

        app.componentNames.foreach(app.startComponent(_))

        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)
        app.startedComponentNames.size should be(app.componentNames.size)

        app.stop()
        app.isRunning() should be(false)
        app.startedComponentNames.isEmpty should be(true)
      } finally {
        app.stop()
      }
    }

    it("can return the names of all components that have been started") {
      val app = createApp()
      try {
        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)
        app.startedComponentNames.size should be(app.componentNames.size)

        app.stopComponent(app.startedComponentNames.head)

        app.startedComponentNames.size should be(app.componentNames.size - 1)
      } finally {
        app.stop()
      }
    }

    it("can tell you if a component has been started") {
      val app = createApp()
      try {
        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)
        app.startedComponentNames.size should be(app.componentNames.size)

        app.stopComponent(app.startedComponentNames.head)

        app.startedComponentNames.size should be(app.componentNames.size - 1)

        app.startedComponentNames.foreach(name => assert(app.isComponentStarted(name)))
      } finally {
        app.stop()
      }
    }

    it("can return the component object class for a started component") {
      val app = createApp()
      try {
        app.start()

        app.isRunning() should be(true)
        app.startedComponentNames.isEmpty should be(false)
        app.startedComponentNames.size should be(app.componentNames.size)

        val compObjects = app.startedComponentNames.map(app.getStartedComponentObjectClass(_))
        compObjects.foreach(c => assert(c.isDefined))

        app.getStartedComponentObjectClass(compA.name).isDefined should be(true)
        app.getStartedComponentObjectClass(compA.name).get //should be(classOf[Comp])

        app.stopComponent(compA.name)
        app.getStartedComponentObjectClass(compA.name).isDefined should be(false)
      } finally {
        app.stop()
      }
    }

    it("can return a component's dependencies") {
      val app = createApp()
      try {
        for {
          comp <- comps
        } yield {
          app.componentDependencies(comp.name) match {
            case Left(l) => throw l
            case Right(dependencies) =>
              val dependencyCount = dependencies.getOrElse(Nil).size
              val expectedNames = comp.dependsOn.getOrElse(Nil).map(_.name).toList
              assert(dependencyCount == expectedNames.size, "Expected does not match actual : %s != %s".format(expectedNames.size, dependencyCount))
              dependencies.foreach { list =>
                list.foreach(name => assert(expectedNames.contains(name)))
              }
          }
        }
      } finally {
        app.stop()
      }

    }

    it("can return a component's dependendents") {
      val app = createApp()
      try {
        for {
          comp <- comps
        } yield {
          app.componentDependents(comp.name) match {
            case Left(l) => throw l
            case Right(dependenciesOption) =>
              dependenciesOption.foreach { dependencies =>
                comp.name match {
                  case compA.name => assert(dependencies.isEmpty, "compA should have no dependents")
                  case compB.name =>
                    val expectedNames = compA.name :: compD.name :: compE.name :: Nil
                    assert(dependencies.size == expectedNames.size, "Expected does not match actual : %s != %s".format(expectedNames.size, dependencies.size))
                    dependencies.foreach(name => assert(expectedNames.contains(name)))
                  case compC.name =>
                  case compD.name =>
                  case compE.name =>
                  case _ =>
                }
              }
          }

        }
      } finally {
        app.stop()
      }

    }

    it("can tell you if the application is running, which is defined as at least one component has started") {
      val app = createApp()
      try {
        for (i <- 1 to 10) {
          app.start()
          app.isRunning should be(true)
          app.stop()
          app.isRunning should be(false)
        }
      } finally {
        app.stop()
      }
    }

    it("""can run all component health checks.""") {
      val app = createApp()
      try {
        app.start()

        app.runComponentHealthChecks() match {
          case Some(healthCheckResultsFutures) =>
            assert(healthCheckResultsFutures.size == 3)

            import scala.concurrent.duration._

            for {
              healthCheckResult <- healthCheckResultsFutures.map(Await.result(_, 1 second))
            } yield {
              log.info(s"healthCheckResult = $healthCheckResult")
              healthCheckResult match {
                case h if (h.healthCheck.name == compA.name) =>
                  assert(h.healthCheckIndicator == GREEN)
                case h if (h.healthCheck.name == compC.name) =>
                  assert(h.healthCheckIndicator == YELLOW)
                case h if (h.healthCheck.name == compE.name) =>
                  assert(h.healthCheckIndicator == RED)
                  assert(h.info.isDefined)
                case _ =>
              }
            }

          case None => throw new IllegalStateException("Expected HealthCheckResults to be returned")
        }

        app.stop()
        assert(app.runComponentHealthChecks().isEmpty)
      } finally {
        app.stop()
      }
    }

    it("can run all application health checks") {
      val app = createApp()
      try {
        app.start()

        app.runApplicationHealthChecks match {
          case Some(futures) =>
            futures.foreach { f =>
              import scala.concurrent.duration._
              val healthCheckResult = Await.result(f, 1 second)
              if (healthCheckResult.healthCheck.name == appHealthCheck1.name) {
                assert(healthCheckResult.healthCheckIndicator == GREEN)
              }
            }

          case None => throw new IllegalStateException("Application health checks should be registered")
        }

        app.stopComponent(compA.name)
        app.runApplicationHealthChecks match {
          case Some(futures) =>
            futures.foreach { f =>
              import scala.concurrent.duration._
              val healthCheckResult = Await.result(f, 1 second)
              if (healthCheckResult.healthCheck.name == appHealthCheck1.name) {
                assert(healthCheckResult.healthCheckIndicator == RED)
                assert(healthCheckResult.info.isDefined)
              }
            }

          case None => throw new IllegalStateException("Application health checks should be registered")
        }
      } finally {
        app.stop()
      }
    }

    it("can run individual component health checks") {
      val app = createApp()
      try {
        app.start()

        app.runComponentHealthChecks(compA.name) match {
          case Left(e) => throw e
          case Right(healthCheckResults) =>
            healthCheckResults match {
              case None => throw new IllegalStateException("Expected health check results for component : " + compA.name)
              case Some(futures) =>
                import scala.concurrent.duration._
                val healthCheckResult = Await.result(futures.head, 1 second)
                assert(healthCheckResult.healthCheckIndicator == GREEN)
            }
        }

        app.stopComponent(compA.name)

        app.runComponentHealthChecks(compA.name) match {
          case Left(e: InvalidComponentNameException) => //expected
          case Right(healthCheckResults) =>
            assert(healthCheckResults.isEmpty, "Since component is stopped, did not expect health checks to be run for component : " + compA.name)
        }

        app.startComponent(compA.name)

        app.runComponentHealthChecks(compA.name) match {
          case Left(e) => throw e
          case Right(healthCheckResults) =>
            assert(healthCheckResults.isDefined, "Expected HealthCheckResults since the component should be started")
        }
      } finally {
        app.stop()
      }
    }

    it("can run individual application health checks") {
      val app = createApp()
      try {
        app.start()

        info("Running health check that will check that CompA is started")
        val healtcheckName = appHealthCheck1.name

        app.runApplicationHealthCheck(healtcheckName) match {
          case Some(f) =>
            import scala.concurrent.duration._
            val healthCheckResult = Await.result(f, 1 second)
            assert(healthCheckResult.healthCheck.name == appHealthCheck1.name)
            assert(healthCheckResult.healthCheckIndicator == GREEN)

          case None => throw new IllegalStateException("Application health checks should be registered")
        }

        info("Stopping component to check that the healthcheck fails.")
        app.stopComponent(compA.name)
        app.runApplicationHealthCheck(healtcheckName) match {
          case Some(f) =>
            import scala.concurrent.duration._
            val healthCheckResult = Await.result(f, 1 second)
            assert(healthCheckResult.healthCheck.name == appHealthCheck1.name)
            assert(healthCheckResult.healthCheckIndicator == RED)
            assert(healthCheckResult.info.isDefined)
          case None => throw new IllegalStateException("Application health checks should be registered")
        }
      } finally {
        app.stop()
      }
    }

    it("can run health checks for a specific group") {
      val app = createApp()
      try {
        app.start()

        app.runAllHealthChecksByGroup(appHealthCheck1.group) match {
          case None => throw new IllegalStateException("Expected health checks for group: " + appHealthCheck1.group)
          case Some(futures) =>
            futures.size should be(3)
        }

        app.stop

        app.runAllHealthChecksByGroup(appHealthCheck1.group) match {
          case None => throw new IllegalStateException("Expected health checks for group: " + appHealthCheck1.group)
          case Some(futures) =>
            assert(futures.size == 2)
        }
      } finally {
        app.stop()
      }
    }

    it("can return all registered application health checks") {
      val app = createApp()
      try {
        val healthChecksBeforeStartingApp = app.applicationHealthChecks.get
        healthChecksBeforeStartingApp.size should be(3)

        app.start()
        healthChecksBeforeStartingApp.size should be(app.applicationHealthChecks.get.size)
      } finally {
        app.stop()
      }
    }

    it("can return health checks for a specific component") {
      val app = createApp()
      try {
        app.start

        app.componentHealthChecks(compA.name).isDefined should be(true)
        app.componentHealthChecks(compB.name).isDefined should be(false)

        app.stop
        app.componentHealthChecks(compA.name).isDefined should be(false)
        app.componentHealthChecks(compB.name).isDefined should be(false)
      } finally {
        app.stop()
      }
    }

    it("can run health checks on a scheduled basis, per the health check config") {
      val app = createApp()
      try {
        app.start()

        val healthCheck = HealthCheck(name = "Heartbeat", schedule = Some(PeriodicTaskSchedule(period = 1)))("logs everytime it runs")

        var healthCheckRunCount = 0

        val healthCheckScorer: (ApplicationService, HealthCheck) => (Int, Option[String]) = (appService, healthCheck) => {
          log.info("RUNNING HEALTHCHECK: \n" + healthCheck)
          healthCheckRunCount += 1
          (100, None)
        }

        app.addHealthCheck(healthCheck, healthCheckRunner(healthCheckScorer.curried(app)))

        Thread.sleep(1005 * 2l)

        app.stop()

        healthCheckRunCount should be >= (2)

        greenCheckScorerRunCount should be > (0)
        val currentGreenCheckScorerRunCount = greenCheckScorerRunCount

        val start = System.currentTimeMillis()
        while (greenCheckScorerRunCount != currentGreenCheckScorerRunCount) {
          if (System.currentTimeMillis() - start > 1000l) {
            greenCheckScorerRunCount should be(currentGreenCheckScorerRunCount)
          }
          Thread.`yield`
        }

      } finally {
        app.stop()
      }
    }

    it("when stopping a component, its dependent components can be shutdown first") {
      val app = createApp()
      try {
        app.start()
        log.info({
          app.componentDependents(compA.name).right.get match {
            case Some(dependsOn) => dependsOn.mkString(compA.name + " DEPENDENTS : ", "|", "")
            case None => compA.name + " DEPENDENTS : "
          }
        })
        app.stopComponent(compA.name, true).isEmpty should be(true)
        log.info("STARTED COMPONENTS AFTER STOPPING A: {}", app.startedComponentNames.mkString("|"))
        app.isComponentStarted(compA.name) should be(false)
        app.isComponentStarted(compB.name) should be(true)
        app.isComponentStarted(compC.name) should be(true)
        app.isComponentStarted(compD.name) should be(true)
        app.isComponentStarted(compE.name) should be(true)

        app.start()
        log.info({
          app.componentDependents(compB.name).right.get match {
            case Some(dependsOn) => dependsOn.mkString(compB.name + " DEPENDENTS : ", "|", "")
            case None => compB.name + " DEPENDENTS : "
          }
        })
        app.stopComponent(compB.name, true).isEmpty should be(true)
        log.info("STARTED COMPONENTS AFTER STOPPING B: {}", app.startedComponentNames.mkString("|"))
        app.isComponentStarted(compA.name) should be(false)
        app.isComponentStarted(compB.name) should be(false)
        app.isComponentStarted(compC.name) should be(true)
        app.isComponentStarted(compD.name) should be(false)
        app.isComponentStarted(compE.name) should be(false)

        app.start()
        log.info({
          app.componentDependents(compC.name).right.get match {
            case Some(dependsOn) => dependsOn.mkString(compC.name + " DEPENDENTS : ", "|", "")
            case None => compC.name + " DEPENDENTS : "
          }
        })
        app.stopComponent(compC.name, true).isEmpty should be(true)
        app.isComponentStarted(compA.name) should be(false)
        app.isComponentStarted(compB.name) should be(false)
        app.isComponentStarted(compC.name) should be(false)
        app.isComponentStarted(compD.name) should be(false)
        app.isComponentStarted(compE.name) should be(false)

        app.start()
        log.info({
          app.componentDependents(compD.name).right.get match {
            case Some(dependsOn) => dependsOn.mkString(compD.name + " DEPENDENTS : ", "|", "")
            case None => compD.name + " DEPENDENTS : "
          }
        })
        app.stopComponent(compD.name, true).isEmpty should be(true)
        log.info("STARTED COMPONENTS AFTER STOPPING D: {}", app.startedComponentNames.mkString("|"))
        app.isComponentStarted(compA.name) should be(false)
        app.isComponentStarted(compB.name) should be(true)
        app.isComponentStarted(compC.name) should be(true)
        app.isComponentStarted(compD.name) should be(false)
        app.isComponentStarted(compE.name) should be(false)

        app.start()
        log.info({
          app.componentDependents(compE.name).right.get match {
            case Some(dependsOn) => dependsOn.mkString(compE.name + " DEPENDENTS : ", "|", "")
            case None => compE.name + " DEPENDENTS : "
          }
        })
        app.stopComponent(compE.name, true).isEmpty should be(true)
        log.info("STARTED COMPONENTS AFTER STOPPING E: {}", app.startedComponentNames.mkString("|"))
        app.isComponentStarted(compA.name) should be(true)
        app.isComponentStarted(compB.name) should be(true)
        app.isComponentStarted(compC.name) should be(true)
        app.isComponentStarted(compD.name) should be(true)
        app.isComponentStarted(compE.name) should be(false)
      } finally {
        app.stop()
      }
    }

  }

}