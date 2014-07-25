package org.olivergg
import org.olivergg.messages._

class Individual(var genes: Array[Int], val fitnessFunc: Array[Int] => Float) extends MyActor {

  var evaluatedFitness: Float = fitnessFunc(genes)

  def receive = {
    case PrintGenes => log.info(genes.mkString(s"genes = {", ":", s"} fitness= ${evaluatedFitness}"))
    case GetFitness => sender ! FitnessResult(this, evaluatedFitness)
    case _ => log.info("Unkwown message received")

  }

  override def toString = genes.mkString("{", ":", "}") + s"($evaluatedFitness)"
}

