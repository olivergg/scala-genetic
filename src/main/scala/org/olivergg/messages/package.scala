package org.olivergg

import akka.actor.ActorPath
package object messages {
  sealed trait Message
  case object GetFitness extends Message
  case class FitnessResult(ind: Individual, value: Float) extends Message
  case object PrintGenes extends Message
  case object Start extends Message
  case object Evolve extends Message
  case class RunTournament(generationPath: ActorPath) extends Message
  case object CreateTournaments extends Message
  case class TournamentResult(ind: Option[Individual]) extends Message
  case object StartCrossing extends Message
  case class CrossingRequest(parent1genes: Individual, parent2genes: Individual) extends Message
  case class CrossingResult(childgenes: Array[Int]) extends Message
  case object SetupRandomGeneration extends Message
  case class GenerationDone(nth: Int) extends Message
  case class AddIndividual(genes: Array[Int]) extends Message
  case class GetBestInd() extends Message
  case class GetBestIndResult(nth:Int, best:Individual) extends Message
  case class MutateInd() extends Message
}