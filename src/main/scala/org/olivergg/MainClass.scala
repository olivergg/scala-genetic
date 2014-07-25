package org.olivergg

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import org.olivergg.messages._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
object MainClass extends App {

  val system = ActorSystem("genetic")

  implicit val timeout = Timeout(4.second)

  val commander = system.actorOf(Props[Population], "commander")

  val setup = commander ! Start

  //  system.scheduler.schedule(0.second, 1.second)(commander ! "watch")

  system.scheduler.scheduleOnce(10.seconds)(system.shutdown)

}