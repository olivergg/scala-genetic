package com.genetic

@deprecated("do not use")
sealed class Individual(val genes: Array[Int])
{
  def apply(idx: Int): Int = genes(idx)

  def update(idx: Int, x: Int):Unit = genes(idx) = x

  def size: Int = genes.length

  override def toString = genes.mkString(";")
}
/**
* Companion object for static methods
*/
@deprecated("do not use")
object Individual
{
  def generateIndividual(size: Int): Individual =
  {
    new Individual(Array.fill(size) {
      scala.util.Random.nextInt(2)
    })
  }
}
@deprecated("do not use")
object EmptyIndividual extends Individual(Array(0))

