/**********************************************************************************************************************
 *                                                   TRACED ACTIONS                                                   *
 **********************************************************************************************************************/

trait IsGuard[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def isGuard = true
}

trait PopsValue[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def popsVStack = true
  override def popsValue = true
}

trait PushesValue[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def pushesVStack = true
  override def pushesValue = true
}

trait RestoresEnv[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def popsVStack = true
  override def restoresEnv = true
}

trait SavesEnv[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def pushesVStack = true
  override def savesEnv = true
}

trait PopsKStack[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def popsKStack = true
}

trait PushesKStack[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def pushesKStack = true
}

trait ChangesValueReg[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def changesValueReg = true
}

trait StartsFunCall[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def startsFunCallBlock = true
}

trait EndsFunCall[Exp, Abs, Addr] extends ActionTrace[Exp, Abs, Addr] {
  override def endsFunCallBlock = true
}

trait RestartPoint[Exp, Abs, Addr]

case class RestartAssertion[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends RestartPoint[Exp, Abs, Addr]
case class RestartFromControl[Exp: Expression, Abs: JoinLattice, Addr: Address](
    newControl: Exp)
    extends RestartPoint[Exp, Abs, Addr]
case class RestartGuardDifferentClosure[Exp: Expression, Abs: JoinLattice, Addr: Address](
    action: ActionStepInT[Exp, Abs, Addr])
    extends RestartPoint[Exp, Abs, Addr]
case class RestartSpecializedPrimitive[Exp: Expression, Abs: JoinLattice, Addr: Address](
    originalPrimitive: Primitive[Addr, Abs],
    n: Int,
    fExp: Exp,
    argsExps: List[Exp])
    extends RestartPoint[Exp, Abs, Addr]
case class RestartTraceEnded[Exp, Abs, Addr]()
    extends RestartPoint[Exp, Abs, Addr]

abstract class ActionGuardT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    val rp: RestartPoint[Exp, Abs, Addr],
    val id: Integer)
    extends ActionTrace[Exp, Abs, Addr]
    with IsGuard[Exp, Abs, Addr]

trait ActionTrace[Exp, Abs, Addr] {
  def isGuard: Boolean = false

  def popsVStack: Boolean = false
  def pushesVStack: Boolean = false

  def restoresEnv: Boolean = false
  def savesEnv: Boolean = false

  def popsValue: Boolean = false
  def pushesValue: Boolean = false

  def popsKStack: Boolean = false
  def pushesKStack: Boolean = false

  def changesValueReg = false

  def startsFunCallBlock = false
  def endsFunCallBlock = false
}

case class ActionAllocVarsT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    varNames: List[String])
    extends ActionTrace[Exp, Abs, Addr]
case class ActionCreateClosureT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    λ: Exp,
    env: Option[Environment[Addr]] = None)
    extends ActionTrace[Exp, Abs, Addr] with ActionReplay[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
case class ActionEndClosureCallT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr]
    with EndsFunCall[Exp, Abs, Addr]
case class ActionEndPrimCallT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr]
    with EndsFunCall[Exp, Abs, Addr]
case class ActionEndTrace[Exp: Expression, Abs: JoinLattice, Addr: Address](
    restartPoint: RestartPoint[Exp, Abs, Addr])
    extends ActionTrace[Exp, Abs, Addr]

/**
  * An error has been reached
  */
case class ActionErrorT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    error: SemanticError)
    extends ActionTrace[Exp, Abs, Addr]
    with ActionReplay[Exp, Abs, Addr]

/**
  * A frame needs to be pushed on the stack, and the interpretation continues by
  * evaluating expression e.
  */
case class ActionEvalPushT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    frame: Frame,
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr]())
    extends ActionTrace[Exp, Abs, Addr]
    with PushesKStack[Exp, Abs, Addr]

/**
  * Evaluation continues with expression e.
  */
case class ActionEvalT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr]())
    extends ActionTrace[Exp, Abs, Addr]
case class ActionExtendEnvT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    varName: String)
    extends ActionTrace[Exp, Abs, Addr]
    with PopsValue[Exp, Abs, Addr]
case class ActionExtendStoreT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    addr: Addr,
    lit: Abs)
    extends ActionTrace[Exp, Abs, Addr]
case class ActionGuardFalseT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    override val rp: RestartPoint[Exp, Abs, Addr],
    override val id: Integer)
    extends ActionGuardT[Exp, Abs, Addr](rp, id)
case class ActionGuardTrueT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    override val rp: RestartPoint[Exp, Abs, Addr],
    override val id: Integer)
    extends ActionGuardT[Exp, Abs, Addr](rp, id)
case class ActionGuardSameClosure[Exp: Expression, Abs: JoinLattice, Addr: Address](
    recordedClosure: Abs,
    override val rp: RestartGuardDifferentClosure[Exp, Abs, Addr],
    override val id: Integer)
    extends ActionGuardT[Exp, Abs, Addr](rp, id)
case class ActionGuardSpecializedPrimitive[Exp: Expression, Abs: JoinLattice, Addr: Address](
    expectedType: SimpleTypes.Value,
    numberOfOperands: Int,
    override val rp: RestartSpecializedPrimitive[Exp, Abs, Addr],
    override val id: Integer)
    extends ActionGuardT[Exp, Abs, Addr](rp, id)
case class ActionLookupVariableT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    varName: String,
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr]())
    extends ActionTrace[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
case class ActionLookupVariablePushT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    varName: String,
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr]())
    extends ActionTrace[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
    with PushesValue[Exp, Abs, Addr]
case class ActionPopKontT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr]
    with PopsKStack[Exp, Abs, Addr] with ActionReplay[Exp, Abs, Addr]
case class ActionPrimCallT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    n: Integer,
    fExp: Exp,
    argsExps: List[Exp],
    fValue: Abs)
    extends ActionTrace[Exp, Abs, Addr] with ActionReplay[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
    with PopsValue[Exp, Abs, Addr] {
  override def popsKont = true
  override def marksFunctionCall = true
}
case class ActionPushValT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr] with ActionReplay[Exp, Abs, Addr]
    with PushesValue[Exp, Abs, Addr]

/**
  * A value is reached by the interpreter. As a result, a continuation will be
  * popped with the given reached value.
  */
case class ActionReachedValueT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    v: Abs,
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr](),
    storeChanges: List[StoreChangeSemantics[Abs, Addr]] = Nil)
    extends ActionTrace[Exp, Abs, Addr] with ActionReplay[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
case class ActionReachedValuePushT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    v: Abs,
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr](),
    storeChanges: List[StoreChangeSemantics[Abs, Addr]] = Nil)
    extends ActionTrace[Exp, Abs, Addr] with ActionReplay[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
    with PushesValue[Exp, Abs, Addr]
case class ActionRemoveKontT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    a: KontAddr,
    k: Kont[KontAddr])
    extends ActionTrace[Exp, Abs, Addr]
case class ActionRestoreEnvT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr]
    with RestoresEnv[Exp, Abs, Addr]
case class ActionRestoreSaveEnvT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr]
    with RestoresEnv[Exp, Abs, Addr]
    with SavesEnv[Exp, Abs, Addr]
case class ActionSaveEnvT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr]
    with SavesEnv[Exp, Abs, Addr]
case class ActionSetVarT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    variable: String)
    extends ActionTrace[Exp, Abs, Addr]
case class ActionSpecializePrimitive[Exp: Expression, Abs: JoinLattice, Addr: Address](
    expectedType: SimpleTypes.Value,
    primitive: Primitive[Addr, Abs],
    n: Integer,
    fExp: Exp,
    argsExps: List[Exp])
    extends ActionTrace[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
    with PopsValue[Exp, Abs, Addr]

/**
  * Similar to ActionEval, but only used when stepping inside a function's body
  * (clo is therefore the function stepped into). The number of arguments should
  * also be provided, as they can be needed by the abstract machine.
  */
case class ActionStepInT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    fexp: Exp,
    e: Exp,
    args: List[String],
    argsv: List[Exp],
    n: Integer,
    frame: Frame,
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr]())
    extends ActionTrace[Exp, Abs, Addr]
    with PopsValue[Exp, Abs, Addr]
    with SavesEnv[Exp, Abs, Addr]
case class ActionStartFunCallT[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionTrace[Exp, Abs, Addr]
    with StartsFunCall[Exp, Abs, Addr]
case class ActionLookupRegisterT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    index: Integer)
    extends ActionTrace[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
case class ActionLookupRegisterPushT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    index: Integer)
    extends ActionTrace[Exp, Abs, Addr]
    with ChangesValueReg[Exp, Abs, Addr]
case class ActionPutRegisterT[Exp: Expression, Abs: JoinLattice, Addr: Address](
    variable: String,
    index: Integer)
    extends ActionTrace[Exp, Abs, Addr]