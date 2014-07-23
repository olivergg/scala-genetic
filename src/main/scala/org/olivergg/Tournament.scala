package org.olivergg

import akka.actor.Actor
import akka.actor.UntypedActor
import akka.event.Logging
import scala.util.Random
import org.olivergg.messages._
import akka.actor.ActorRef
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.collection.mutable.Seq
import scala.concurrent.Future
import scala.concurrent.Await
import scala.collection.mutable.ListBuffer
import akka.actor.ActorSelection
import akka.actor.ActorSystem
import akka.pattern.pipe

class Tournament(tournamentSize: Int, populationSize: Int) extends MyActor {

  def receive = {
    case RunTournament(activeGenerationPath) => {
      val futureSeq = ListBuffer[Future[FitnessResult]]()
      for (i <- 1 to tournamentSize) {
        val randomId = randomIndex()
        val randInd = context.system.actorSelection(activeGenerationPath./(s"ind_$randomId"))
        val f = (randInd ? GetFitness).mapTo[FitnessResult]
        futureSeq += f
      }
      val ff = Future.sequence(futureSeq)
      val origSender = sender
      ff.onSuccess {
        case results => {
          val maxRes = results.maxBy(fit => fit.value)
          origSender ! TournamentResult(Some(maxRes.ind))
          context.stop(self)
        }
      }

    }
    case _ => log.info("Unkwown message received")

  }

  def randomIndex() = (Random.nextFloat() * (populationSize - 1)).toInt + 1
}