package org.olivergg

import akka.actor.Actor
import org.olivergg.messages._
import scala.util.Random
import akka.event.Logging

class Crossing extends MyActor {
  val crossProbability = 0.6f

  def receive = {
    case CrossingRequest(p1, p2) => {
      log.debug(s"Received crossing request for parent p1=$p1 and p2=$p2")
      sender ! CrossingResult(crossInd(p1, p2))
    }
    case _ => log.info("Unkwown message received")

  }

  def crossInd(parent1: Individual, parent2: Individual): Array[Int] =
    {
      val n = parent1.genes.size
      val child = Array.fill(n)(0)
      for (i <- child.indices) {
        val randomValue = Random.nextFloat()
        if (randomValue <= crossProbability) {
          child(i) = parent1.genes(i)
        } else {
          child(i) = parent2.genes(i)
        }
      }
      child
    }
}