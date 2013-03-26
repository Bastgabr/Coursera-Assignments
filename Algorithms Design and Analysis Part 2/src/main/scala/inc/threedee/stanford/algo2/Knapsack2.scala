package inc.threedee.stanford.algo2

import collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 05/01/13
 * Time: 12:46 PM
 * To change this template use File | Settings | File Templates.
 */


object Knapsack2 extends Knapsack {
  def main(args: Array[String]): Unit = {
    println(solveKnapsack("/inc/threedee/stanford/algo2/knapsack2.txt"))
  }

  def solveKnapsack(items: ArrayBuffer[KnapsackItem], knapsackSize: Int, numberOfItems: Int): Int = {
    val solution: Array[Array[Int]] = Array.ofDim[Int](numberOfItems + 1, knapsackSize + 1)

    def getValue(a: Int, b: Int): Int = {
      if (solution(a)(b) == -1) {
        if (items(a).weight <= b) {
          solution(a)(b) = math.max(getValue(a - 1, b), getValue(a - 1, b - items(a).weight) + items(a).value)
        }
        else {
          solution(a)(b) = getValue(a - 1, b)
        }
      }
      if (solution(a)(b) == -1) {
        solution(a)(b) = 0
      }
      solution(a)(b)
    }

    for (x <- 0 to knapsackSize) {
      solution(0)(x) = 0
    }

    for (i <- 1 to numberOfItems) {
      for (x <- 0 to knapsackSize) {
        solution(i)(x) = -1
      }
    }

    getValue(numberOfItems, knapsackSize)
  }
}