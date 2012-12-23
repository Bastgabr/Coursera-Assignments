package inc.threedee.stanford.algo2

import collection.mutable.ArrayBuffer
import io.Source
import util.control.Breaks._

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 22/12/12
 * Time: 9:40 PM
 * To change this template use File | Settings | File Templates.
 */

case class Edge(startNode: Int, endNode: Int, cost: Int)

object Clustering1 {
  def main(args: Array[String]): Unit = {
    val rowsFromFile: List[String] = List[String](Source.fromInputStream(getClass.getResourceAsStream("/inc/threedee/stanford/algo2/clustering1.txt")).getLines().toList: _*)
    val numNodes: Int = rowsFromFile.head.toInt
    val edges: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()
    val distances: Array[Array[Int]] = Array.ofDim[Int](numNodes + 1, numNodes + 1)
    var clusterToNodes: Map[Int, Set[Int]] = Map[Int, Set[Int]]()
    var nodeToCluster: Map[Int, Int] = Map[Int, Int]()

    for (i <- 1 to numNodes) {
      clusterToNodes += i -> Set[Int](i)
      nodeToCluster += i -> i
    }

    for (row <- rowsFromFile.tail) {
      val rowParts: Array[Int] = row.split(' ').map(_.toInt)
      val (startNode, endNode, cost): (Int, Int, Int) = (rowParts(0), rowParts(1), rowParts(2))
      edges += Edge(startNode, endNode, cost)
      distances(startNode)(endNode) = cost
    }

    val sortedEdges: ArrayBuffer[Edge] = edges.sortWith((e1, e2) => e1.cost < e2.cost)

    val k: Int = 4 // Desired number of clusters

    breakable {
      for (edge <- sortedEdges) {
        /**
         * Repeat until there are only k clusters left
         */

        if (clusterToNodes.size <= k) {
          break;
        }

        val p: Int = edge.startNode
        val q: Int = edge.endNode
        val pCluster = nodeToCluster.get(p).get
        val qCluster = nodeToCluster.get(q).get
        if (pCluster != qCluster) {
          /**
           * If p & q are closest pair of separated points
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

    var minSpacing: Int = Int.MaxValue

    for (edge <- edges) {
      val p: Int = edge.startNode
      val q: Int = edge.endNode
      if (nodeToCluster.get(p).get != nodeToCluster.get(q).get && edge.cost < minSpacing) {
        minSpacing = edge.cost
      }
    }

    println(minSpacing)
  }
}

