package inc.threedee.stanford.algo2

import io.Source
import collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 05/01/13
 * Time: 12:46 PM
 * To change this template use File | Settings | File Templates.
 */

case class KnapsackItem(value: Int, weight: Int)

object Knapsack1 {
  def main(args: Array[String]): Unit = {
    val rowsFromFile: List[String] = List[String](Source.fromInputStream(getClass.getResourceAsStream("/inc/threedee/stanford/algo2/knapsack1.txt")).getLines().toList: _*)
    val headerRowParts: Array[Int] = rowsFromFile.head.split(' ').map(_.toInt)
    val (knapsackSize, numberOfItems): (Int, Int) = (headerRowParts(0), headerRowParts(1))
    val items: ArrayBuffer[KnapsackItem] = new ArrayBuffer[KnapsackItem](numberOfItems + 1)
    items += KnapsackItem(0, 0) //Dummy
    for (row <- rowsFromFile.tail) {
      val rowParts: Array[Int] = row.split(' ').map(_.toInt)
      val item: KnapsackItem = KnapsackItem(rowParts(0), rowParts(1))
      items += item
    }
    println(solveKnapsack(items, knapsackSize, numberOfItems))
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