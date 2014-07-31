package org.olivergg

import akka.actor.Props
import akka.pattern.ask
import com.genetic.KnapsackProblem
import org.olivergg.messages._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class Generation(nth: Int, popSize: Int) extends MyActor {

  var generationSize = 0

  def receive = {
    case SetupRandomGeneration => {
      log.info("setup random generation")
      for (i <- 1 to popSize) {
        self ! AddIndividual(Array.fill(KnapsackProblem.objets.size) {
          scala.util.Random.nextInt(2)
        })
      }
    }

    case AddIndividual(somegenes: Array[Int]) => {
      val ind = context.actorOf(Props(new Individual(somegenes, KnapsackProblem.getFitness(_))), s"ind_${generationSize}")
      ind ! MutateInd
      generationSize += 1
      if (generationSize == popSize) {
        log.info(s"generation $nth is done")
        context.parent ! GenerationDone(nth)
      }
    }

    case GetBestInd => {
      val listOfFutures = ListBuffer[Future[FitnessResult]]()
      for (i <- 0 until generationSize) {
        val ind = context.actorSelection(self.path./(s"ind_$i"))
        val f = (ind ? GetFitness).mapTo[FitnessResult]
        listOfFutures += f
      }
      val wait = Future.sequence(listOfFutures)
      sender ! GetBestIndResult(nth, Await.result(wait, 30.seconds).maxBy(fr => fr.value).ind)
    }

    case _ => log.info("Unkwown message received")

  }
}