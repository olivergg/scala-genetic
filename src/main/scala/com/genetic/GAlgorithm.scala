package com.genetic

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
@deprecated("do not use")
object GAlgorithm extends App {
  val crossProbability = 0.6f
  val mutationProbability = 0.07f
  val tournamentSize = 10
  val SELECTIVE_PRESSURE = 1.9f

  def evolve(pop: Population): Population =
    {
      pop.computeFitnesses(KnapsackProblem.getFitness)

      //      val parents = proportionalRouletteWheelSelection(pop)

      val parents = Array.fill(pop.size)(tournamentSelection(pop))

      val newPop = Population(pop.size, pop.geneSize)

      var i = 0
      for (ind: Array[Individual] <- parents.sliding(2)) {
        val ind1 = ind(0)
        val ind2 = ind(1)
        val newInd = crossInd(ind1, ind2)

        newPop.saveIndividual(i, newInd)
        i += 1
      }
      newPop.saveIndividual(i, parents(parents.length - 1))

      //    for (i <- 0 until pop.size) {
      //      val ind1 = tournamentSelection(pop)
      //      val ind2 = tournamentSelection(pop)
      //      val newInd = crossInd(ind1, ind2)
      //      newPop.saveIndividual(i, newInd)
      //    }

      for (i <- 0 until pop.size) {
        mutateInd(newPop.getIndividual(i))
      }
      newPop.saveIndividual(0, pop.getFittest())
      newPop
    }

  def crossInd(parent1: Individual, parent2: Individual): Individual =
    {
      require(parent1.size == parent2.size)
      val n = parent1.size
      val child = new Individual(Array.fill(n)(0))
      for (i <- child.genes.indices) {
        val randomValue = Random.nextFloat()
        if (randomValue <= crossProbability) {
          child(i) = parent1(i)
        } else {
          child(i) = parent2(i)
        }
      }
      child
    }

  def mutateInd(p1: Individual): Unit =
    {
      for (i <- p1.genes.indices) {
        if (Random.nextFloat() <= mutationProbability) {
          p1(i) = Random.nextInt(2)
        }
      }
    }

  // Tournament selection
  def tournamentSelection(pop: Population): Individual =
    {
      val tournament = Population(tournamentSize, pop.geneSize)
      for (i <- 0 until tournamentSize) {
        val randomId = (Random.nextFloat() * pop.size).asInstanceOf[Int]
        tournament.saveIndividual(i, pop.getIndividual(randomId))
      }
      tournament.getFittest()
    }

  def proportionalRouletteWheelSelection(pop: Population): Array[Individual] =
    {
      val sumFit = pop.sumFitness()
      val indsFitnesses = pop.inds.map(ind => pop.f(ind))
      val cumulativeProb = new ArrayBuffer[Float](pop.size)
      var cumulativeFitness = 0f
      for (fit <- indsFitnesses) {
        cumulativeFitness += fit
        cumulativeProb.append(cumulativeFitness)
      }
      val output = new ArrayBuffer[Individual](pop.size)

      while (output.size < pop.size) {
        val idx = cumulativeProb.indexWhere(prob => prob >= Random.nextFloat() * sumFit)
        if (idx > -1) output.append(pop.inds(idx))
      }
      output.toArray
    }

  def rankBasedRouletteWheelSelection(pop: Population): Array[Individual] =
    {
      //    println(pop.inds.mkString(";"))

      val indsFitnesses = pop.inds.sortBy(ind => pop.f(ind))

      def rank(pos: Int, n: Int): Float =
        {
          2 - SELECTIVE_PRESSURE + (2 * (SELECTIVE_PRESSURE - 1) * pos.toFloat / (n - 1))
        }

      val indsRanks = new Array[Float](pop.size)

      for (i <- indsFitnesses.indices) {
        indsRanks(i) = rank(i, indsFitnesses.length)
        //      println("i = " + i + "; individu = " + indsFitnesses(i) + " fitness = " + f(indsFitnesses(i)) + "  rank = " + indsRanks(i))
      }

      //    for (trip <- (pop.inds, indsFitnesses, indsRanks).zipped.toList) {
      //      println(trip)
      //    }
      val sumRank = indsRanks.sum
      //    println("sumFit = " + sumRank)
      //    println("indsFitnesses = " + indsFitnesses.mkString(";"))

      val cumulativeProb = new ArrayBuffer[Float](pop.size)
      var cumulativeFitness = 0f
      for (fit <- indsRanks) {
        cumulativeFitness += fit
        cumulativeProb.append(cumulativeFitness)
      }
      //    println("cumulativeProb = " + cumulativeProb)
      val output = new ArrayBuffer[Individual](pop.size)

      while (output.size < pop.size) {
        println("still in the loop")
        val idx = cumulativeProb.indexWhere(prob => prob >= Random.nextFloat() * sumRank)
        if (idx > -1 && pop.f(indsFitnesses(idx)) < Float.PositiveInfinity) {
          output.append(indsFitnesses(idx))
        }
      }
      output.toArray
    }

  //// Main code below
//  for (i <- 1 to 5) {

    var myPop = Population(50, KnapsackProblem.objets.length, true)
    //val inds = Array(Individual(Array(0, 1, 1, 0, 0, 1)), Individual(Array(0, 1, 0, 1, 1, 0)), Individual(Array(1, 0, 1, 0, 0, 0)), Individual(Array(1, 0, 0, 0, 0, 1)), Individual(Array(0, 0, 0, 0, 1, 0)), Individual(Array(0, 1, 0, 0, 1, 0)), Individual(Array(1, 1, 0, 1, 0, 1)), Individual(Array(1, 0, 1, 0, 1, 1)), Individual(Array(1, 0, 0, 0, 0, 1)), Individual(Array(1, 1, 0, 1, 1, 1)))
    //var myPop = new Population(10, FitnessCalc.objets.size, false, inds)

    //rankBasedRouletteWheelSelection(myPop).foreach(x => println(s"$x => ${f(x) * FitnessCalc.maxPrice}"))
    //println(myPop)

    var sol: Individual = EmptyIndividual

    //val randomInd = tournamentSelection(myPop)

    for (i <- 1 to 50) {
      //  while (f(sol) <= FitnessCalc.minPriceSol / FitnessCalc.maxPrice) {
      //    myPop.inds.foreach(ind => print(" " + ind))
      //    println("")
      //    println(myPop.inds.map(ind => FitnessCalc.getFitness(ind)).groupBy(x => x).map(f => (f._1, f._2.length)))


      myPop = GAlgorithm.evolve(myPop)
      println(s"$i.th generation = $myPop")
      println(myPop.fitnessesMap)
    }

    println(myPop.inds.groupBy(x => x.toString).map(y => (y._1, y._2.length)))
    //println(myPop.inds.map(ind => FitnessCalc.getFitness(ind)).groupBy(x => x).map(f => (f._1, f._2.length)))
    myPop.computeFitnesses(KnapsackProblem.getFitness)

    sol = myPop.getFittest()

    //  def mulElementWise[T](a: Array[T], b: Array[T], mul: (T, T) => T): Array[T] = {
    //    val c :Array[T] = Array.ofDim(a.length)
    //    for (i <- 0 until a.length) {
    //      c(i) = mul(a(i), b(i))
    //    }
    //    c
    //  }

    import KnapsackProblem.BagObject
    def getPrice(objets: Array[BagObject], sol: Individual): Float = {
      val zipped = objets zip sol.genes
      val mapped = zipped.map { case (obj, gene) => obj.price * gene }
      mapped.sum
    }

    def getWeight(objets: Array[BagObject], sol: Individual): Float = {
      val zipped = objets zip sol.genes
      val mapped = zipped.map { case (obj, gene) => obj.weight * gene }
      mapped.sum
    }

    println(KnapsackProblem.objets.mkString(";"))
    println(s"gene size = ${KnapsackProblem.objets.length}")
    println(s"max price that can be obtain = ${KnapsackProblem.maxPrice}")
    println("mympop fittest = " + sol + "  sol fitness = " + myPop.f(sol))
    println("sol price = " + getPrice(KnapsackProblem.objets, sol))
    println("sol weight = " + getWeight(KnapsackProblem.objets, sol))

//  }
}
