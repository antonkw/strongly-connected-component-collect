import java.util.Scanner

import CollectSCC.AdjList
import scala.collection.AbstractSeq
import scala.collection.mutable.ArrayBuffer


object CollectSCC extends App {
  type AdjList = ArrayBuffer[Int]

  val scanner = new Scanner(System.in)
  val n = scanner.nextInt
  val m = scanner.nextInt

  def scanDirectedGraph(vertexesCount: Int, edgesCount: Int): Array[AdjList] = {
    val graph = Array.fill[AdjList](vertexesCount)(new AdjList)

    for (i <- 0 until edgesCount) {
      val x = scanner.nextInt
      val y = scanner.nextInt

      graph(x - 1) += (y - 1)
    }

    graph
  }

  val graph = scanDirectedGraph(n, m)

  new Thread(null, new Solve(graph), "1", 536870912).start()
}

class Solve extends Runnable {
  def this(x: Array[AdjList]) = {
    this()
    this.array = x
  }

  var array: Array[AdjList] = _

  override def run(): Unit = {
    println(new GraphUtils().collectSCCs(this.array).length)
  }
}


class GraphUtils {
  def collectSCCs(adj: Array[AdjList]): List[AbstractSeq[Int]] = {
    val reversedGraph = reverseGraph(adj)
    val postVisitingValues = dfsExplore(reversedGraph)
    val sortedByPostValues = postVisitingValues.zipWithIndex.sortBy(_._1).reverse.map(_._2).toList
    collectAllSCCs(sortedByPostValues, adj, sortedByPostValues)
  }

  private def reverseGraph(adjList: Array[AdjList]) = {
    val reversed = Array.fill[AdjList](adjList.length)(new AdjList)

    for (i <- adjList.indices) {
      val adj = adjList(i)
      adj.foreach(idx => reversed(idx) += i)
    }

    reversed
  }

  private def dfsExplore(graph: Array[AdjList]): Array[Int] = {
    val vertexesCount = graph.length
    val explored = Array.fill[Boolean](vertexesCount)(false)

    val notExploredVertexesBuffer = new ArrayBuffer[Int](vertexesCount)
    for (i <- graph.indices) notExploredVertexesBuffer += i

    var counter = 1
    val postVisitingValues = Array.ofDim[Int](vertexesCount)

    def depthFirstExploring(x: Int = 0): Unit = {
      val adjList = graph(x)
      explored(x) = true
      notExploredVertexesBuffer -= x

      for (x <- adjList if !explored(x)) {
        depthFirstExploring(x)
      }

      postVisitingValues(x) = counter
      counter += 1
    }

    while (notExploredVertexesBuffer.nonEmpty) {
      depthFirstExploring(notExploredVertexesBuffer(0))
    }

    postVisitingValues
  }

  private def collectAllSCCs(sortedVertixes: List[Int], graph: Array[AdjList], vertexesSortedByPostVals: List[Int]) = {
    val explored = Array.fill[Boolean](graph.length)(false)

    def collectSCC(x: Int = 0, graph: Array[AdjList]): ArrayBuffer[Int] = {
      val exploredBuffer = new ArrayBuffer[Int]

      def collectSccHelper(x: Int): Unit = {
        val adjList = graph(x)
        exploredBuffer += x
        explored(x) = true

        for (x <- adjList if !explored(x)) {
          collectSccHelper(x)
        }
      }

      collectSccHelper(x)

      exploredBuffer
    }


    def collectAllSccHelper(collectedSCCs: List[AbstractSeq[Int]], counter: Int): List[AbstractSeq[Int]] = {
      if (counter == vertexesSortedByPostVals.length) {
        collectedSCCs
      } else {
        if (explored(vertexesSortedByPostVals(counter))) {
          collectAllSccHelper(collectedSCCs, counter + 1)
        } else {
          val scc = collectSCC(vertexesSortedByPostVals(counter), graph)
          collectAllSccHelper(scc :: collectedSCCs, counter + 1)
        }
      }
    }

    collectAllSccHelper(List(), 0)
  }


}