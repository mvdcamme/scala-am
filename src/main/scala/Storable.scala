/**
  * Created by mvdcamme on 21/03/16.
  */
class Storable[Abs : AbstractValue, Addr : Address](combo: Either[Abs, Environment[Addr]]) {

  def getVal: Abs = combo match {
    case Left(value) =>
      value
    case Right(env) =>
      throw new IncorrectStorableException(s"Environment $env is not a Hybridvalue")
  }

  def getEnv: Environment[Addr] = combo match {
    case Left(value) =>
      throw new IncorrectStorableException(s"Hybridvalue $value is not an environment")
    case Right(env) =>
      env
  }
}

case class StoreVal[Abs : AbstractValue, Addr : Address](value: Abs) extends Storable[Abs, Addr](Left(value))
case class StoreEnv[Abs : AbstractValue, Addr : Address](env: Environment[Addr]) extends Storable[Abs, Addr](Right(env))

case class IncorrectStorableException(message: String) extends Exception(message)
