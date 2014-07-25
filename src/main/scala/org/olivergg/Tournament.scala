package org.olivergg

import akka.pattern.ask
import org.olivergg.messages._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

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