import backend.Path

trait ConcreteStatesStore {
  type Key = (SchemeExp, Int)
  protected def toKey(exp: SchemeExp, state: ConcolicMachineState): Key = {
    (exp, state.store.keys.size)
  }
  def addState(exp: SchemeExp, state: ConcolicMachineState, path: Path): Unit
  def containsState(exp: SchemeExp, state: ConcolicMachineState, path: Path, stateComparer: ConcolicMachineStateComparer): Boolean
}

class MultiConcreteStatesStore extends ConcreteStatesStore {
  private var concreteStates: Map[Key, Set[(ConcolicMachineState, Path)]] = Map()
  def addState(exp: SchemeExp, state: ConcolicMachineState, path: Path): Unit = {
    val key = toKey(exp, state)
    val existingStates = concreteStates.getOrElse(key, Set())
    concreteStates += key -> (existingStates + ((state, path)))
  }
  def containsState(exp: SchemeExp, state: ConcolicMachineState, path: Path, stateComparer: ConcolicMachineStateComparer): Boolean = {
    val statesWithSameControl = concreteStates.getOrElse(toKey(exp, state), Set())
    statesWithSameControl.exists(tuple => {
      /*
       * The current path should also be different from the path that was recorded for the other state.
       * If this would not be taken into account, it would be impossible to explore the subtree starting
       * below the point where the state was stored.
       */
      statesWithSameControl.exists(tuple => {
        stateComparer.equalModuloTimestamp(state, tuple._1) && tuple._2 != path
      })
    })
  }
}

class SingleConcreteStatesStore extends ConcreteStatesStore {
  private var concreteStates: Map[Key, (ConcolicMachineState, Path)] = Map()
  def addState(exp: SchemeExp, state: ConcolicMachineState, path: Path): Unit = {
    val key = toKey(exp, state)
    concreteStates += key -> (state, path)
  }
  def containsState(exp: SchemeExp, state: ConcolicMachineState, path: Path, stateComparer: ConcolicMachineStateComparer): Boolean = {
    val statesWithSameControl = concreteStates.get(toKey(exp, state))
    statesWithSameControl.exists(tuple => {
      /*
       * The current path should also be different from the path that was recorded for the other state.
       * If this would not be taken into account, it would be impossible to explore the subtree starting
       * below the point where the state was stored.
       */
      statesWithSameControl.exists(tuple => {
        stateComparer.equalModuloTimestamp(state, tuple._1) && tuple._2 != path
      })
    })
  }
}