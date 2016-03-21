/**
  * Created by mvdcamme on 21/03/16.
  */
class Storable(combo : Either[HybridLattice.Hybrid, Environment[HybridAddress]]) {

  def getVal : HybridLattice.Hybrid = combo match {
    case Left(value) =>
      value
    case Right(env) =>
      throw new IncorrectStorableException(s"Environment $env is not a Hybridvalue")
  }

  def getEnv : Environment[HybridAddress] = combo match {
    case Left(value) =>
      throw new IncorrectStorableException(s"Hybridvalue $value is not an environment")
    case Right(env) =>
      env
  }
}

case class StoreVal(value : HybridLattice.Hybrid) extends Storable(Left(value))
case class StoreEnv(env : Environment[HybridAddress]) extends Storable(Right(env))

case class IncorrectStorableException(message : String) extends Exception(message)
