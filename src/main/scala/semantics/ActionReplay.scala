trait ActionReplay[Exp, Abs, Addr] {
  def popsKont: Boolean = false
  def ticksTime: Boolean = false
}

/*
 * Extend store with the given addresses, initialized to the bottom value.
 */
case class ActionAllocAddressesR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    addresses: List[Addr])
    extends ActionReplay[Exp, Abs, Addr]

case class ActionClosureCallMarkR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    override val fExp: Exp,
    override val fValue: Abs,
    lambda: Exp)
    extends ActionFunCallMarkR[Exp, Abs, Addr](fExp, fValue)

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
    with PushesKStack[Exp, Abs, Addr]

/**
  * To be used when saving a frame that stores data.
  */
case class ActionEvalPushDataR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    e: Exp,
    env: Environment[Addr],
    frameGenerator: Abs => Frame)
    extends ActionReplay[Exp, Abs, Addr]
    with PushesKStack[Exp, Abs, Addr]

abstract class ActionFunCallMarkR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    val fExp: Exp,
    val fValue: Abs)
    extends ActionReplay[Exp, Abs, Addr]

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