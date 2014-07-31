package org.olivergg

import org.olivergg.messages._

import scala.util.Random

class Individual(var genes: Array[Int], val fitnessFunc: Array[Int] => Float) extends MyActor {

  val mutationProbability = 0.01
  var evaluatedFitness: Float = fitnessFunc(genes)

  def receive = {
    case PrintGenes => log.info(genes.mkString(s"genes = {", ":", s"} fitness= ${evaluatedFitness}"))
    case GetFitness => sender ! FitnessResult(this, evaluatedFitness)
    case MutateInd => {
      for (i <- genes.indices) {
        if (Random.nextFloat() <= mutationProbability) {
          log.debug("############# MUTATION OCCURED FOR "+self.path)
          genes(i) = Random.nextInt(2)
        }
      }
      evaluatedFitness = fitnessFunc(genes)
    }
    case _ => log.info("Unkwown message received")

  }

  override def toString = genes.mkString("{", ":", "}") + s"($evaluatedFitness)"
}

