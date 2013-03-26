package inc.threedee.stanford.algo2

import collection.mutable.{BitSet, ArrayBuffer}
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 23/12/12
 * Time: 11:42 AM
 * To change this template use File | Settings | File Templates.
 */

case class Node(bitSet: BitSet)

object Clustering2 {
  def main(args: Array[String]): Unit = {
    val rowsFromFile: List[String] = List[String](Source.fromInputStream(getClass.getResourceAsStream("/inc/threedee/stanford/algo2/clustering2.txt")).getLines().toList: _*)
    val headerRowParts: Array[String] = rowsFromFile.head.split(' ')
    val numNodes: Int = headerRowParts(0).toInt
    val numBitsInEachNodeLabel: Int = headerRowParts(1).toInt
    val nodes: ArrayBuffer[Node] = new ArrayBuffer[Node](numNodes + 1)
    var clusterToNodes: Map[Int, Set[Int]] = Map[Int, Set[Int]]()
    var nodeToCluster: Map[Int, Int] = Map[Int, Int]()

    for (i <- 1 to numNodes) {
      clusterToNodes += i -> Set[Int](i)
      nodeToCluster += i -> i
    }

    nodes += Node(BitSet())
    for (row <- rowsFromFile.tail) {
      val bitsForNode: Array[Int] = row.split(' ').map(_.toInt)
      val bitSet: BitSet = BitSet()
      for (i <- 0 until numBitsInEachNodeLabel) {
        if (bitsForNode(i) == 1) bitSet.add(i)
      }
      nodes += Node(bitSet)
    }

    for (i <- 1 to numNodes) {
      for (j <- (1 + 1) to numNodes) {
        val distance: Int = (nodes(i).bitSet ^ nodes(j).bitSet).toList.size
        if (distance <= 2) {
          val pCluster = nodeToCluster.get(i).get
          val qCluster = nodeToCluster.get(j).get
          if (pCluster != qCluster) {
            /**
             * If p & q are a close pair of separated points
             * Then merge the clusters containing p & q into a single cluster
             */
            val pClusterNodes: Set[Int] = clusterToNodes.get(pCluster).get
            val qClusterNodes: Set[Int] = clusterToNodes.get(qCluster).get

            qClusterNodes.foreach(nodeId => {
              nodeToCluster += nodeId -> pCluster
            })
            clusterToNodes += pCluster -> (pClusterNodes union qClusterNodes)
            clusterToNodes -= qCluster
          }
        }
      }
    }

    println(clusterToNodes.size)
  }
}