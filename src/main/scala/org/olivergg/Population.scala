package org.olivergg

import akka.actor.Actor
import akka.actor.ActorRef
import org.olivergg.messages._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.UntypedActor
import akka.actor.Props
import com.genetic.ProblemeSacADosDesc
import akka.event.Logging
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.util.Random
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer
class Population extends MyActor {

  val populationSize = 50

  val crossing = context.actorOf(Props[Crossing], name = s"crossing")

  var generationNth = 0

  var activeGeneration: ActorRef = _

  var nextGenParents: ListBuffer[Individual] = ListBuffer()

  var startTimeStamp: Long = 0

  def stopCondition(): Boolean = generationNth >= 4

  def receive = {
    case Start => {
      log.info("start population ")
      startTimeStamp = System.nanoTime()
      activeGeneration = context.actorOf(Props(new Generation(generationNth, populationSize)), s"generation_$generationNth")
      activeGeneration ! SetupRandomGeneration

    }

    case Evolve => {
      log.info("received evolve message")
      if (!stopCondition()) {
        for (i <- 1 to populationSize + 1) {
          val tourn = context.actorOf(Props(new Tournament(10, populationSize)), name = s"gen_${generationNth}_tournament_$i")
          tourn ! RunTournament(activeGeneration.path)
        }
      } else {
        log.info(s"Total duration = ${(System.nanoTime() - startTimeStamp) / 1E6} ms")
      }
    }

    case TournamentResult(res) => {
      res match {
        case None => log.info("No winner for tournament")
        case Some(winnerInd) => {
          log.debug(s"tournament result = ${winnerInd.self.actorRef.path.name}")
          val winnerFitness = winnerInd.evaluatedFitness
          log.debug(s"Gen $generationNth  => Winner fitness is $winnerFitness")
          nextGenParents += winnerInd
          if (nextGenParents.size == populationSize + 1) {
            log.info(s"switching to crossing mode")
            context.become(crossingMode)
            self ! StartCrossing
          }
        }
      }
    }

    case GenerationDone(nth) if nth == 0 => {
      log.info(s"generation 0 is done ")
      self ! Evolve
    }
    case _ => log.info("Unkwown message received")
  }

  def crossingMode: Receive = {
    case StartCrossing => {
      println("######################## crossing ##################")
      println(s"parents size is now = ${nextGenParents.size}")
      println("#####################################################")
      generationNth += 1
      activeGeneration = context.actorOf(Props(new Generation(generationNth, populationSize)), s"generation_$generationNth")
      log.info("###########################")
      log.info(s"nextGenParents size ${nextGenParents.size}")
      for (ind <- nextGenParents.sliding(2)) {
        val parent1 = ind(0)
        val parent2 = ind(1)
        crossing ! CrossingRequest(parent1, parent2)
      }

    }

    case CrossingResult(childGenes) => {
      activeGeneration ! AddIndividual(childGenes)

    }

    case GenerationDone(nth) => {
      log.info(s"generation $nth is done")
      if (!nextGenParents.isEmpty) {
        val bestInd = nextGenParents.maxBy(i => i.evaluatedFitness)
        log.info(s"best ind of generation n°$generationNth is = $bestInd")
      }
      nextGenParents.clear
      context.become(receive)
      self ! Evolve
    }
    case _ => log.info("Unkwown message received")
  }

}