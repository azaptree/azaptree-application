package com.azaptree.application.event

import scala.concurrent.ExecutionContext

import akka.event.japi.SubchannelEventBus
import akka.util.Subclassification

/**
 * Event are published to Subscribers synchronously on the publisher's thread
 */
class SynchronousSubchannelEventBus extends SubchannelEventBus[Any, Any => Unit, Class[_]] {
  object ClassSubclassification extends Subclassification[Classifier] {
    override def isEqual(x: Class[_], y: Class[_]): Boolean = x == y

    override def isSubclass(x: Class[_], y: Class[_]): Boolean = y.isAssignableFrom(x)

  }

  override def classify(event: Event): Classifier = event.getClass

  override def publish(event: Event, subscriber: Subscriber) = subscriber(event)

  override implicit def subclassification: Subclassification[Classifier] = ClassSubclassification

}

