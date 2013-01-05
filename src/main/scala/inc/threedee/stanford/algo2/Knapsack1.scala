package inc.threedee.stanford.algo2

import collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 05/01/13
 * Time: 12:46 PM
 * To change this template use File | Settings | File Templates.
 */


object Knapsack1 extends Knapsack {
  def main(args: Array[String]): Unit = {
    println(solveKnapsack("/inc/threedee/stanford/algo2/knapsack1.txt"))
  }

  def solveKnapsack(items: ArrayBuffer[KnapsackItem], knapsackSize: Int, numberOfItems: Int): Int = {
    val solution: Array[Array[Int]] = Array.ofDim[Int](numberOfItems + 1, knapsackSize + 1)
    for (x <- 0 to knapsackSize) {
      solution(0)(x) = 0
    }
    for (i <- 1 to numberOfItems) {
      for (x <- 0 to knapsackSize) {
        if (items(i).weight <= x) {
          solution(i)(x) = math.max(solution(i - 1)(x), solution(i - 1)(x - items(i).weight) + items(i).value)
        }
        else {
          solution(i)(x) = solution(i - 1)(x)
        }
      }
    }
    solution(numberOfItems)(knapsackSize)
  }
}