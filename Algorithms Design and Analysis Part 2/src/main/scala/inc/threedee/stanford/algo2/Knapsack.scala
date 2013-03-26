package inc.threedee.stanford.algo2

import collection.mutable.ArrayBuffer
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 05/01/13
 * Time: 1:42 PM
 * To change this template use File | Settings | File Templates.
 */

case class KnapsackItem(value: Int, weight: Int)

trait Knapsack {

  def solveKnapsack(fileName: String): Int = {
    val (items, knapsackSize, numberOfItems): (ArrayBuffer[KnapsackItem], Int, Int) = parseInput(fileName)
    solveKnapsack(items, knapsackSize, numberOfItems)
  }

  def solveKnapsack(items: ArrayBuffer[KnapsackItem], knapsackSize: Int, numberOfItems: Int): Int

  private def parseInput(fileName: String): (ArrayBuffer[KnapsackItem], Int, Int) = {
    val rowsFromFile: List[String] = List[String](Source.fromInputStream(getClass.getResourceAsStream(fileName)).getLines().toList: _*)
    val headerRowParts: Array[Int] = rowsFromFile.head.split(' ').map(_.toInt)
    val (knapsackSize, numberOfItems): (Int, Int) = (headerRowParts(0), headerRowParts(1))
    val items: ArrayBuffer[KnapsackItem] = new ArrayBuffer[KnapsackItem](numberOfItems + 1)
    items += KnapsackItem(0, 0) //Dummy
    for (row <- rowsFromFile.tail) {
      val rowParts: Array[Int] = row.split(' ').map(_.toInt)
      val item: KnapsackItem = KnapsackItem(rowParts(0), rowParts(1))
      items += item
    }
    (items, knapsackSize, numberOfItems)
  }
}