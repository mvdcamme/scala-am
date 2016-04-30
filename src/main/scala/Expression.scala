trait Expression[A] {
  def zeroExp: A
}

object Expression {
  implicit object ANFExpExpression extends Expression[ANFExp] {
    def zeroExp = ANFValue(ValueInteger(0))
  }
  implicit object SchemeExpExpression extends Expression[SchemeExp] {
    def zeroExp = SchemeValue(ValueInteger(0))
  }
}
