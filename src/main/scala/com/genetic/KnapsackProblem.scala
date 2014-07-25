package com.genetic

object KnapsackProblem {

  case class Objet(weight: Int, price: Int) {
    override def toString: String = s"[w=$weight,p=$price]"
  }

  val objets = Array(
    Objet(12, 6),
    Objet(2, 3),
    Objet(1, 2),
    Objet(1, 4),
    Objet(3, 1),
    Objet(11, 5),
    Objet(3, 2))

  val maxPrice = objets.map(obj => obj.price).sum

  val BAG_MAX_WEIGHT = 14

  def getFitness(ind: Individual): Float =
    {
      getFitness(ind.genes)
    }

  def getFitness(genes: Array[Int]): Float =
    {
      var out = 0f
      var totalPrice = 0.0f
      var totalWeight = 0.0f
      for (i <- genes.indices) {
        totalPrice += genes(i) * objets(i).price
        totalWeight += genes(i) * objets(i).weight
      }
      if (totalWeight > 0 && totalWeight <= BAG_MAX_WEIGHT) {
        out = totalPrice /// totalWeight /// maxPrice
      } else {
        out = 0
      }
      out
    }
}
