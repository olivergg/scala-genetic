package org.olivergg

import akka.actor.Props
import com.genetic.KnapsackProblem
import org.olivergg.messages._
class Generation(nth: Int, popSize: Int) extends MyActor {

  var generationSize = 0

  def receive = {
    case SetupRandomGeneration => {
      log.info("setup random generation")
      for (i <- 1 to popSize) {
        self ! AddIndividual(Array.fill(KnapsackProblem.objets.size) { scala.util.Random.nextInt(2) })
      }
    }

    case AddIndividual(somegenes: Array[Int]) => {
      val ind = context.actorOf(Props(new Individual(somegenes, KnapsackProblem.getFitness(_))), s"ind_${generationSize}")
      generationSize += 1
      if (generationSize == popSize) {
        log.info(s"generation $nth is done")
        context.parent ! GenerationDone(nth)
      }
    }

    case _ => log.info("Unkwown message received")

  }
}