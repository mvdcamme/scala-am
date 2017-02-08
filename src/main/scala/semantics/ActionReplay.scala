trait ActionReplay[Exp, Abs, Addr]

/*
 * Extend store with the given addresses, initialized to the bottom value.
 */
case class ActionAllocAddressesR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    addresses: List[Addr])
    extends ActionReplay[Exp, Abs, Addr]

/*
 * Extend store with these addresses and initialize them to their corresponding value on the stack.
 */
case class ActionDefineAddressesR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    addresses: List[Addr])
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

case class ActionLookupAddressR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    a: Addr)
    extends ActionReplay[Exp, Abs, Addr]


case class ActionSetAddressR[Exp: Expression, Abs: JoinLattice, Addr: Address](
    adress: Addr)
    extends ActionReplay[Exp, Abs, Addr]