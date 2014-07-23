package org.olivergg

import akka.actor.{ ActorRef, Props, ActorSystem }
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import org.olivergg.messages._
import com.genetic.ProblemeSacADosDesc
import akka.pattern.ask
import akka.util.Timeout
object MainClass extends App {

  val system = ActorSystem("genetic")

  implicit val timeout = Timeout(4.second)

  val commander = system.actorOf(Props[Population], "commander")

  val setup = commander ! Start

  //  system.scheduler.schedule(0.second, 1.second)(commander ! "watch")

  system.scheduler.scheduleOnce(10.seconds)(system.shutdown)

}