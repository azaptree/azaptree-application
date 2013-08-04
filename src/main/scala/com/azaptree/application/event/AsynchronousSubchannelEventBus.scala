package com.azaptree.application.event

import scala.concurrent.ExecutionContext

/**
 * Event are published to Subscribers asynchronously on the publisher's thread
 */
class AsynchronousSubchannelEventBus extends SynchronousSubchannelEventBus {

  override def publish(event: Event, subscriber: Subscriber) = {
    ExecutionContext.global.execute(new Runnable() { def run = subscriber(event) })
  }

}