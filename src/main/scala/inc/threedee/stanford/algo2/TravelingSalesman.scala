package inc.threedee.stanford.algo2

import io.Source
import collection.mutable.ArrayBuffer
import collection.mutable.BitSet
import collection.Iterator

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 19/01/13
 * Time: 10:27 PM
 * To change this template use File | Settings | File Templates.
 */

case class Coordinate(x: Double, y: Double)

object Coordinate {
  private var distanceCache: Map[(Coordinate, Coordinate), Double] = Map[(Coordinate, Coordinate), Double]()

  def getDistance(c1: Coordinate, c2: Coordinate): Double = {
    distanceCache.get((c1, c2)) match {
      case None => {
        val distance: Double = math.sqrt((c1.x - c2.x) * (c1.x - c2.x) - (c1.y - c2.y) * (c1.y - c2.y))
        distanceCache += (c1, c2) -> distance
        distance
      }
      case Some(distance) => {
        distance
      }
    }

  }
}

object TravelingSalesman {
  private val INFINITY: Double = 10000000

  def computeMinTravelingSalesmanTourCost(numCities: Int, cityCoordinates: ArrayBuffer[Coordinate]): Double = {
    var setToStoredCostIndexedByDestinationIndex: Map[BitSet, Array[Double]] = Map[BitSet, Array[Double]]()
    for (i <- 1 to numCities) {
      val storedCost: Array[Double] = Array.fill(numCities) {
        INFINITY
      }
      i match {
        case 1 => storedCost(0) = 0
        case _ =>
      }
      setToStoredCostIndexedByDestinationIndex += BitSet(i) -> storedCost
    }
    for (m <- 2 to numCities) {
      println("m = " + m)
      val subsetsOfSizeMContaining1: Iterator[BitSet] =
        (1 to numCities).combinations(m).filter(set => set.contains(1)).map(elemsInSet => {
          BitSet(elemsInSet.toArray: _*)
        })
      for (set <- subsetsOfSizeMContaining1) {
        val updatedCostsForSetIndexedByDestinationIndex: Array[Double] = new Array[Double](numCities)
        for (j <- set.filter(_ != 1)) {
          var min: Double = INFINITY
          val setMinusJ: BitSet = set - j
          val storedCostsForSetMinusJ: Array[Double] = setToStoredCostIndexedByDestinationIndex.get(setMinusJ).get
          for (k <- set.filter(_ != j)) {
            val currCost: Double = storedCostsForSetMinusJ(k - 1) match {
              case INFINITY => INFINITY
              case _ => storedCostsForSetMinusJ(k - 1) +
                Coordinate.getDistance(cityCoordinates(k - 1), cityCoordinates(j - 1))
            }
            if (currCost < min) {
              min = currCost
            }
          }
          updatedCostsForSetIndexedByDestinationIndex(j - 1) = min
        }
        setToStoredCostIndexedByDestinationIndex += set -> updatedCostsForSetIndexedByDestinationIndex
      }
      setToStoredCostIndexedByDestinationIndex = setToStoredCostIndexedByDestinationIndex.filterKeys(_.size == m)
    }
    var totalMinCost: Double = INFINITY
    val completeSet: BitSet = BitSet((1 to numCities).toArray: _*)
    val storedCostsForCompleteSets: Array[Double] = setToStoredCostIndexedByDestinationIndex.get(completeSet).get
    for (j <- 2 to numCities) {
      val currCost: Double = storedCostsForCompleteSets(j - 1) match {
        case INFINITY => INFINITY
        case _ => storedCostsForCompleteSets(j - 1) +
          Coordinate.getDistance(cityCoordinates(j - 1),
            cityCoordinates(0))
      }
      if (currCost < totalMinCost) {
        totalMinCost = currCost
      }
    }
    totalMinCost
  }

  def main(args: Array[String]): Unit = {
    val rowsFromFile: List[String] = List[String](Source.fromInputStream
      (getClass.getResourceAsStream("/inc/threedee/stanford/algo2/tsp.txt")).getLines().toList: _*)
    val numCities: Int = rowsFromFile.head.toInt
    val cityCoordinates: ArrayBuffer[Coordinate] = new ArrayBuffer[Coordinate](numCities)
    for (row <- rowsFromFile.tail) {
      val rowParts: Array[Double] = row.split(' ').map(_.toDouble)
      cityCoordinates += Coordinate(rowParts(0), rowParts(1))
    }

    println(computeMinTravelingSalesmanTourCost(numCities, cityCoordinates).toInt)
  }
}