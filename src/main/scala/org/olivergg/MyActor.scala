package org.olivergg

import akka.actor.Actor
import akka.event.Logging
import akka.util.Timeout
import scala.concurrent.duration._
trait MyActor extends Actor {
  val log = Logging(context.system, this)

  implicit val timeout = Timeout(4.second)

}