package test.com.azaptree.application.component

import com.azaptree.application.component.ComponentInitialized
import com.azaptree.application.component.ComponentStopped
import com.azaptree.application.component.ComponentLifeCycle
import org.scalatest.matchers.ShouldMatchers
import com.azaptree.application.component.ComponentStarted
import com.azaptree.application.component.Component
import org.scalatest.FunSpec
import com.azaptree.application.component.ComponentConstructed
import com.azaptree.application.component.ComponentNotConstructed
import ComponentSpec._
import scala.util.Try
import com.azaptree.logging.Slf4jLogger

object ComponentSpec {
  val started = "ComponentStarted"
  val initialized = "ComponentInitialized"
  val constructed = "ComponentConstructed"

  case class CompA(state: List[String] = Nil) {
    def addState(newState: String): CompA = {
      copy(state = newState :: state)
    }
  }

  class CompALifeCycle extends ComponentLifeCycle[CompA] {
    protected def create(comp: Component[ComponentNotConstructed, CompA]): Component[ComponentConstructed, CompA] = {
      Component[ComponentConstructed, CompA](name = "CompA", componentLifeCycle = this, componentObject = Some(CompA(constructed :: Nil)))
    }

    override protected def init(comp: Component[ComponentConstructed, CompA]): Component[ComponentInitialized, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentInitialized, CompA](componentObject = Some(compA.addState(initialized)))
    }

    override protected def start(comp: Component[ComponentInitialized, CompA]): Component[ComponentStarted, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentStarted, CompA](componentObject = Some(compA.addState(started)))
    }

    override protected def stop(comp: Component[ComponentStarted, CompA]): Component[ComponentStopped, CompA] = {
      comp.copy[ComponentStopped, CompA](componentObject = None)
    }
  }

  class InvalidCreateCompALifeCycle extends ComponentLifeCycle[CompA] {
    protected def create(comp: Component[ComponentNotConstructed, CompA]): Component[ComponentConstructed, CompA] = {
      Component[ComponentConstructed, CompA](name = "CompA", componentLifeCycle = this)
    }

    override protected def init(comp: Component[ComponentConstructed, CompA]): Component[ComponentInitialized, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentInitialized, CompA](componentObject = Some(compA.addState(initialized)))
    }

    override protected def start(comp: Component[ComponentInitialized, CompA]): Component[ComponentStarted, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentStarted, CompA](componentObject = Some(compA.addState(started)))
    }

    override protected def stop(comp: Component[ComponentStarted, CompA]): Component[ComponentStopped, CompA] = {
      comp.copy[ComponentStopped, CompA](componentObject = None)
    }
  }

  class InvalidInitCompALifeCycle extends ComponentLifeCycle[CompA] {
    protected def create(comp: Component[ComponentNotConstructed, CompA]): Component[ComponentConstructed, CompA] = {
      Component[ComponentConstructed, CompA](name = "CompA", componentLifeCycle = this, componentObject = Some(CompA(constructed :: Nil)))
    }

    override protected def init(comp: Component[ComponentConstructed, CompA]): Component[ComponentInitialized, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentInitialized, CompA](componentObject = None)
    }

    override protected def start(comp: Component[ComponentInitialized, CompA]): Component[ComponentStarted, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentStarted, CompA](componentObject = Some(compA.addState(started)))
    }

    override protected def stop(comp: Component[ComponentStarted, CompA]): Component[ComponentStopped, CompA] = {
      comp.copy[ComponentStopped, CompA](componentObject = None)
    }
  }

  class InvalidStartCompALifeCycle extends ComponentLifeCycle[CompA] {
    protected def create(comp: Component[ComponentNotConstructed, CompA]): Component[ComponentConstructed, CompA] = {
      Component[ComponentConstructed, CompA](name = "CompA", componentLifeCycle = this, componentObject = Some(CompA(constructed :: Nil)))
    }

    override protected def init(comp: Component[ComponentConstructed, CompA]): Component[ComponentInitialized, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentInitialized, CompA](componentObject = Some(compA.addState(initialized)))
    }

    override protected def start(comp: Component[ComponentInitialized, CompA]): Component[ComponentStarted, CompA] = {
      val compA = comp.componentObject.get
      comp.copy[ComponentStarted, CompA](componentObject = None)
    }

    override protected def stop(comp: Component[ComponentStarted, CompA]): Component[ComponentStopped, CompA] = {
      comp.copy[ComponentStopped, CompA](componentObject = None)
    }
  }

}

class ComponentSpec extends FunSpec with ShouldMatchers with Slf4jLogger {

  describe("A Component") {
    it("can shutdown itself using it's registered ComponentLifeCycle") {
      val compA = Component[ComponentNotConstructed, CompA]("CompA", new CompALifeCycle())
      val compAStarted = compA.startup()
      compAStarted.componentObject.get.state should have size (3)
      compAStarted.componentObject.get.state should be((started :: initialized :: constructed :: Nil))

      val compAStopped = compA.shutdown()
      info("After the component is shutdown, its componentObject should be None")
      compAStopped.componentObject.isEmpty should be(true)
    }

    it("the ComponentLifeCycle create(), init(), and start() must return a Component that has a componentObject instance") {
      var compA = Component[ComponentNotConstructed, CompA]("CompA", new InvalidCreateCompALifeCycle())
      var startupResult = Try {
        compA.startup()
      }

      startupResult.isFailure should be(true)

      compA = Component[ComponentNotConstructed, CompA]("CompA", new InvalidInitCompALifeCycle())
      startupResult = Try {
        compA.startup()
      }

      startupResult.isFailure should be(true)

      compA = Component[ComponentNotConstructed, CompA]("CompA", new InvalidStartCompALifeCycle())
      startupResult = Try {
        compA.startup()
      }

      startupResult.isFailure should be(true)
    }

    it("requires a componentObject when the state is not ComponentNotConstructed or ComponentStopped ") {
      Component[ComponentNotConstructed, CompA]("CompA", new CompALifeCycle())
      Try {
        Component[ComponentNotConstructed, CompA]("CompA", new CompALifeCycle(), Some(CompA(constructed :: Nil)))
      }.isFailure should be(true)

      Component[ComponentStopped, CompA]("CompA", new CompALifeCycle())
      Try {
        Component[ComponentStopped, CompA]("CompA", new CompALifeCycle(), Some(CompA(constructed :: Nil)))
      }.isFailure should be(true)

      Component[ComponentConstructed, CompA]("CompA", new CompALifeCycle(), Some(CompA(constructed :: Nil)))
      Try {
        Component[ComponentConstructed, CompA]("CompA", new CompALifeCycle())
      }.isFailure should be(true)

      Component[ComponentInitialized, CompA]("CompA", new CompALifeCycle(), Some(CompA(constructed :: Nil)))
      Try {
        Component[ComponentInitialized, CompA]("CompA", new CompALifeCycle())
      }.isFailure should be(true)

      Component[ComponentStarted, CompA]("CompA", new CompALifeCycle(), Some(CompA(constructed :: Nil)))
      Try {
        Component[ComponentStarted, CompA]("CompA", new CompALifeCycle())
      }.isFailure should be(true)

    }

  }

}