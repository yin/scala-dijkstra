import scala.collection.mutable._

class Dijkstra[G <: WeightedGraph](graph: G) {
  type Node = G#Node
  type Edge = G#Edge
  /**
   * StopCondition provides a way to terminate the algorithm at a certain
   * point, e.g.: When target becomes settled.
   */
  type StopCondition = (Set[Node], Map[Node, Int], Map[Node, Node])
  	=> Boolean
  
  /**
   * By default the Dijkstra algorithm processes all nodes reachable from
   * <code>start</code> given to <code>compute()</code>.
   */
  val defaultStopCondition: StopCondition = (_, _, _) => true
  var stopCondition = defaultStopCondition

  def compute(start: Node, target: Node):
	  (Map[Node, Int], Map[Node, Node]) = {
    var queue: Set[Node] = new HashSet()
    var settled: Set[Node] = new HashSet()
    var distance: Map[Node, Int] = new HashMap()
    var path: Map[Node, Node] = new HashMap()
    queue += start
    distance(start) = 0
    
    while(!queue.isEmpty && stopCondition(settled, distance, path)) {
      val u = extractMinimum(queue, distance)
      settled += u
      relaxNeighbors(u, queue, settled, distance, path)
    }
    
    return (distance, path)
  }

  /**
   * Finds element of <code>Q</code> with minimum value in D, removes it
   * from Q and returns it.
   */
  protected def extractMinimum[T](Q: Set[T], D: Map[T, Int]): T = {
    var u = Q.first
    Q.foreach((node) =>  if(D(u) > D(node)) u = node)
    Q -= u
    return u;
  }
  
  /**
   * For all nodes <code>v</code> not in <code>S</code>, neighbors of
   * <code>u</code>}: Updates shortest distances and paths, if shorter than
   * the previous value.
   */
  protected def relaxNeighbors(u: Node, Q: Set[Node], S: Set[Node],
      D: Map[Node, Int], P: Map[Node, Node]): Unit = {
      for(edge <- graph.edges if(edge.a == u || edge.b == u) ) {
        var v = if(edge.a == u) edge.b else edge.a
        if(!S.contains(v)) {
          if(!D.contains(v) || D(v) > D(u) + edge.getWeight) {
            D(v) = D(u) + edge.getWeight
            P(v) = u
            Q += v
          }
        }
      }

  }
}

object Dijkstra {
  /**
   * Test case:
   *    ____________(5)
   *   /   9           \
   * (6)_____           \6
   *  |      \2          \
   *  |       \          |
   *  |     ---(3)-----(4)
   *  |14  /    |        |
   *  |   /     |        |
   *  |  / 9    |10      |
   *  | /       |        |
   * (1)-------(2)-------'
   *      7         15 
   * 
   */
  def main(args: Array[String]): Unit = {
    // 1. Construct graph
    val g = new WeightedGraph(1)
    val n1 = g.addNode
    val n2 = g.addNode
    val n3 = g.addNode
    val n4 = g.addNode
    val n5 = g.addNode
    val n6 = g.addNode
    n1.connectWith(n2).setWeight(7)
    n1.connectWith(n3).setWeight(9)
    n1.connectWith(n6).setWeight(14)
    n2.connectWith(n3).setWeight(10)
    n2.connectWith(n4).setWeight(15)
    n3.connectWith(n4).setWeight(11)
    n3.connectWith(n6).setWeight(2)
    n4.connectWith(n5).setWeight(6)
    n5.connectWith(n6).setWeight(9)
    
    // 2. Set start, target, stop-condition and compute the path
    val (start, target) = (n1, n5)
    val dijkstra = new Dijkstra[g.type](g)
    // 2.1. Halt when target becomes settled
    dijkstra.stopCondition = (S, D, P) => !S.contains(target)
    val (distance, path) = dijkstra.compute(start, target)
    
    // 3. Display the result
    printResult[g.type](start, target, distance, path)
  }
  
  def printResult[G <: Graph](start: G#Node, target: G#Node,
      distance: Map[G#Node, Int], path: Map[G#Node, G#Node]): Unit = {
    var shortest = List(target)
    while(shortest.head != start) {
      shortest ::= path(shortest.head)
    }
    println("Shortest-path cost: " + distance(target))
    print("Shortest-path: " + shortest.mkString(" -> "))
  }
}