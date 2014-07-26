package com.genetic

object KnapsackProblem {

  case class BagObject(weight: Int, price: Int) {
    override def toString: String = s"[w=$weight,p=$price]"
  }

  val objets = Array(
    BagObject(12, 6),
    BagObject(2, 3),
    BagObject(1, 2),
    BagObject(1, 4),
    BagObject(3, 1),
    BagObject(11, 5),
    BagObject(3, 2),
    BagObject(3, 2))

  val maxPrice = objets.map(obj => obj.price).sum

  val BAG_MAX_WEIGHT = 18

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
