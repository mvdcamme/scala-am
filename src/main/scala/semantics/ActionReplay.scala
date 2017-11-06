import backend._

trait ActionReplay[Exp, Abs, Addr] {
  def marksFunctionCall: Boolean = false
  def popsKont: Boolean = false
  def pushesKontAtExp: Option[Exp] = None
  def ticksTime: Boolean = false
}

trait FrameGenerator[Abs] {
  def apply(value: Abs, optConcolicValue: Option[ConcolicExpression], frame: Frame): Frame
}

/*
 * Extend store with the given addresses, initialized to the bottom value.
 */
case class ActionAllocAddressesR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    addresses: List[Addr])
    extends ActionReplay[Exp, Abs, Addr]

/*
 * Signals that a closure must be called.
 */
case class ActionClosureCallR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    fExp: Exp,
    lambda: Exp,
    env: Environment[Addr])
    extends ActionReplay[Exp, Abs, Addr] {
  override def marksFunctionCall = true
}

/*
 * Extend store with these addresses and initialize them to their corresponding value on the stack.
 * Also pop the topmost continuation frame from the stack.
 */
case class ActionDefineAddressesPopR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    addresses: List[Addr])
    extends ActionReplay[Exp, Abs, Addr] {
  override def popsKont = true
}

/*
 * Extend store with these addresses and initialize them to their corresponding value on the stack.
 */
case class ActionDefineAddressesR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    addresses: List[Addr])
    extends ActionReplay[Exp, Abs, Addr]

/**
  * Evaluation continues with expression e in environment env.
  */
case class ActionEvalR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    env: Environment[Addr],
    read: Set[Addr] = Set[Addr](),
    write: Set[Addr] = Set[Addr]())
    extends ActionReplay[Exp, Abs, Addr]

/**
  * frame is pushed on the stack, and the interpretation continues by evaluating expression e in environment ρ.
  */
case class ActionEvalPushR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    env: Environment[Addr],
    frame: Frame)
    extends ActionReplay[Exp, Abs, Addr]
    with PushesKStack[Exp, Abs, Addr] {
  override def pushesKontAtExp = Some(e)
}

/**
  * To be used when saving a frame that stores data.
  */
case class ActionEvalPushDataR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    env: Environment[Addr],
    frameGenerator: FrameGenerator[Abs])
    extends ActionReplay[Exp, Abs, Addr]
    with PushesKStack[Exp, Abs, Addr] {
  override def popsKont = true
  override def pushesKontAtExp = Some(e)
}

case class ActionLookupAddressR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    a: Addr)
    extends ActionReplay[Exp, Abs, Addr]


case class ActionSetAddressR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    adress: Addr)
    extends ActionReplay[Exp, Abs, Addr]

case class ActionTimeTickR[Exp: Expression, Abs: JoinLattice, Addr: Address]()
    extends ActionReplay[Exp, Abs, Addr] {
    override def ticksTime = true
}

case class ActionTimeTickExpR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    exp: Exp)
    extends ActionReplay[Exp, Abs, Addr] {
    override def ticksTime = true
}