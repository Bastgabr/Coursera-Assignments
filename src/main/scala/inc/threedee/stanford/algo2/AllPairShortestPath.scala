package inc.threedee.stanford.algo2

import io.Source
import util.control.Breaks._

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 14/01/13
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */

object AllPairShortestPath {
  private def computeShortestPath(fileName: String): Int = {
    val rowsFromFile: List[String] = List[String](Source.fromInputStream(getClass.getResourceAsStream(fileName)).getLines().toList: _*)
    val headerRowParts: Array[Int] = rowsFromFile.head.split(' ').map(_.toInt)
    val numVertices: Int = headerRowParts(0)
    val graph: Array[Array[Int]] = Array.fill(numVertices, numVertices) {
      Int.MaxValue / 2
    }
    for (v <- 0 until numVertices) {
      graph(v)(v) = 0
    }
    for (row <- rowsFromFile.tail) {
      val rowParts: Array[Int] = row.split(' ').map(_.toInt)
      val tail: Int = rowParts(0)
      val head: Int = rowParts(1)
      val length: Int = rowParts(2)
      graph(tail - 1)(head - 1) = length
    }

    for (k <- 0 until numVertices) {
      for (i <- 0 until numVertices) {
        for (j <- 0 until numVertices) {
          if ((graph(i)(k) + graph(k)(j)) < graph(i)(j)) {
            graph(i)(j) = (graph(i)(k) + graph(k)(j))
          }
        }
      }
    }

    var shortest: Int = Int.MaxValue
    breakable {
      for (v <- 0 until numVertices) {
        if (graph(v)(v) < 0) {
          shortest = 0
          break
        }
      }
    }

    if (shortest != 0) {
      for (i <- 0 until numVertices) {
        for (j <- 0 until numVertices) {
          if (i != j && graph(i)(j) < shortest) {
            shortest = graph(i)(j)
          }
        }
      }
    }

    shortest
  }

  def main(args: Array[String]): Unit = {
    val min1: Int = computeShortestPath("/inc/threedee/stanford/algo2/graph1.txt")
    val min2: Int = computeShortestPath("/inc/threedee/stanford/algo2/graph2.txt")
    val min3: Int = computeShortestPath("/inc/threedee/stanford/algo2/graph3.txt")
    val mins: List[Int] = List[Int](min1, min2, min3)
    val noNegativeCostCycles: List[Int] = mins.filter(min => min != 0)
    val output: String = noNegativeCostCycles.size match {
      case 0 => "NULL"
      case _ => noNegativeCostCycles.min.toString
    }
    println(output)
  }
}