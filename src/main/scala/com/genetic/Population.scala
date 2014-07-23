package com.genetic

import scala.collection.immutable.HashMap


class Population(val initialSize: Int, val geneSize: Int, initialize: Boolean = false, private var individuals: Array[Individual])
{
  var fitnessesMap: Map[Individual, Float] = HashMap.empty

  def inds: Array[Individual] = individuals

  if (initialize)
  {
    individuals = Array.fill(initialSize)(Individual.generateIndividual(geneSize))
  }

  def size: Int = individuals.length

  def getFittest(): Individual = individuals.maxBy(ind => f(ind))

  def sumFitness(): Float = individuals.map(ind => f(ind)).sum

  def getIndividual(idx: Int): Individual = individuals(idx)

  def saveIndividual(idx: Int, ind: Individual): Unit = individuals(idx) = ind

  def computeFitnesses(afitness: Individual => Float): Unit =
  {
    fitnessesMap = individuals.map(ind => (ind, afitness(ind))).toMap
  }

  def getComputedFitness(ind: Individual): Float = fitnessesMap.getOrElse(ind, 0f)

  def f(ind: Individual): Float = getComputedFitness(ind)

  override def toString = individuals.mkString("[","|","]")

}

/**
* Companion object
*/
object Population
{
  def apply(initialSize: Int, geneSize: Int, initialize: Boolean = false): Population =
  {
    new Population(initialSize, geneSize, initialize, Array.fill(initialSize)(EmptyIndividual))
  }
}
