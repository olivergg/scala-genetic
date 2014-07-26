package org.olivergg

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import akka.event.Logging
import akka.util.Timeout

/**
 * A trait that aims to replace the Actor trait to add some common and implicit values.
 */
trait MyActor extends Actor {
  val log = Logging(context.system, this)

  implicit val timeout = Timeout(4, TimeUnit.SECONDS)

}