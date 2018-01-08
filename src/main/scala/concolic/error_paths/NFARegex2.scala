import dk.brics.automaton.State

class NFARegex2(graph : Graph[State, String, Unit], initialState: State, states: Array[State], finalStates : List[State]) {

  val regexSimplifier = new RegexLang

  def hasEdge(node1: State, annot: String, node2: State): Boolean = {
    graph.nodeEdges(node1).contains((annot, node2))
  }

  //Transitive closure method
  def compute2(): Set[Regex] = {
    val size = states.length
    val A = Array.ofDim[Regex](size + 1, size + 1, size + 1)
    val annotations = graph.getAnnotations.toList

    for (i <- 1 to size) {
      for (j <- 1 to size) {
        if (i == j) {
          A(i)(j)(0) = EmptyWord
        } else {
          A(i)(j)(0) = EmptySet
        }

        for (a <- annotations) {
          if (hasEdge(states(i-1), a, states(j-1))) {
            A(i)(j)(0) = regexSimplifier.simplifyr(Or(A(i)(j)(0), Atomic(a)))
          }
        }
      }
    }

    println("starting transitive closure")

    var start = System.nanoTime()
    for(k <- 1 to size) {
      for(i <- 1 to size) {
        for(j <- 1 to size) {
          A(i)(j)(k) = regexSimplifier.simplifyr(Or(A(i)(j)(k-1), Concat(A(i)(k)(k-1), Concat(Star(A(k)(k)(k-1)), A(k)(j)(k-1)))))
        }
      }
    }

    println("Took "+((System.nanoTime - start) / Math.pow(10, 9))+"s to do iterations")


    val initial = states.indexOf(initialState) + 1

    for(i <- 1 to size) {
      if(i != initial) A(i) = null
      if(!finalStates.contains(states(i-1))) {
        //A(initial)(i) = null
      }
    }

    var regexes = Set[Regex]()

    for(i <- 1 to size) {
      if(finalStates.contains(states(i-1))) {
        println("writing final state "+(i-1))
        start = System.nanoTime()
        val r = sum(A(initial)(i)(size))
        println("Took "+((System.nanoTime - start) / Math.pow(10, 9))+"s")
        println("size = "+r.size)
        regexes = regexes ++ r
        A(initial)(i) = null
      }
    }

    regexes
  }

  def listRegexes(regex: Regex):Set[Regex] = regex match {
    case Or(x, y) => listRegexes(x) ++ listRegexes(y)
    //And(x, y)
    case x => Set(x)
  }


  def sum(regex: Regex):  Set[Regex] = {
    def sumAcc(trees : List[Regex], acc : Set[Regex]):  Set[Regex] = trees match {
      case Nil => acc
      case Or(x, y) :: rs => sumAcc(x :: y :: rs, acc)
      case x :: rs => sumAcc(rs, Set(x) ++ acc)

    }
    sumAcc(List(regex), Set())
  }

}