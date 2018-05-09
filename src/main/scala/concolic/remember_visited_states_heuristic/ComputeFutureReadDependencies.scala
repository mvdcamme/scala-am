object ComputeFutureReadDependencies {

  def computeFutureReadDependencies[N, Addr: Address](node: N, graph: Graph[N, EdgeAnnotation[_, _, Addr], _]): Set[Addr] = {
    def readsAddr(edgeAnnot: EdgeAnnotation[_, _, Addr]): Set[Addr] = edgeAnnot.actions.collect({
      case ActionLookupAddressR(address) => address
    }).toSet
    @scala.annotation.tailrec
    def loop(todo: Set[N], visitedNodes: Set[N], collectedAddressesRead: Set[Addr]): Set[Addr] = todo.headOption match {
      case None => collectedAddressesRead
      case Some(node) if visitedNodes.contains(node) => loop(todo.tail, visitedNodes, collectedAddressesRead)
      case Some(node) =>
        val edges = graph.nodeEdges(node)
        val newTodo = todo.tail ++ edges.map(_._2)
        val newCollectedVarsRead = edges.foldLeft(collectedAddressesRead)((acc, edge) => {
          readsAddr(edge._1) ++ acc
        })
        loop(newTodo, visitedNodes + node, newCollectedVarsRead)
    }
    loop(Set(node), Set(), Set())
  }

}
