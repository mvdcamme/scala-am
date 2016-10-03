trait ConvertedControl[Exp, Abs, Addr] {
  def toString(store: Store[Addr, Abs]): String = toString()
}

case class ConvertedControlEval[Exp: Expression,
                                Abs: JoinLattice,
                                Addr: Address](e: Exp, ρ: Environment[Addr])
    extends ConvertedControl[Exp, Abs, Addr] {
  override def toString = s"ev($e)"
}

case class ConvertedControlError[Exp: Expression, Abs: JoinLattice,
Addr: Address](reason: SemanticError)
    extends ConvertedControl[Exp, Abs, Addr] {
  override def toString = s"err($reason)"
}

case class ConvertedControlKont[Exp: Expression, Abs: JoinLattice,
Addr: Address](v: Abs)
    extends ConvertedControl[Exp, Abs, Addr] {
  override def toString = s"ko($v)"
}
