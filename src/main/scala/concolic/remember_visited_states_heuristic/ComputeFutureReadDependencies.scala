class ComputeFutureReadDependencies[N, Addr: Address] {

  private var cache: Map[N, Set[Addr]] = Map()
  private def lookupInCache(node: N): Option[Set[Addr]] = cache.get(node)
  private def storeInCache(node: N, addresses: Set[Addr]): Unit = {
    cache += node -> addresses
  }

  def computeFutureReadDependencies(nodes: Set[N], graph: Graph[N, EdgeAnnotation[_, _, Addr], _]): Set[Addr] = {
    def readsAddr(edgeAnnot: EdgeAnnotation[_, _, Addr]): Set[Addr] = edgeAnnot.effects.collect({
      case EffectReadConsCar(address) => address
      case EffectReadConsCdr(address) => address
      case EffectReadVariable(address) => address
      case EffectReadVector(address) => address
    })
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
    nodes.flatMap(node => {
      lookupInCache(node) match {
        case Some(addresses) => addresses
        case None =>
          val computedAddresses = loop(Set(node), Set(), Set())
          storeInCache(node, computedAddresses)
          computedAddresses
      }
    })
  }

}
