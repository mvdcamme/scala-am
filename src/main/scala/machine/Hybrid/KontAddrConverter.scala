trait KontAddrConverter {

  def convertKontAddr[KAddr <: KontAddr](k: KAddr): KAddr

}

object IdKontAddrConverter extends KontAddrConverter {

  def convertKontAddr[KAddr <: KontAddr](k: KAddr): KAddr = k

}

/*
 * To be used for converting continuation addresses: delegates to the proper conversion strategy.
 */
class DefaultKontAddrConverter[Exp: Expression] extends KontAddrConverter {

  val timestampConverter = IdHybridTimestampConverter
  val kontAddressConverter = IdKontAddrConverter

  def convertKontAddr[KAddr <: KontAddr](k: KAddr): KAddr =
    kontAddressConverter.convertKontAddr(k)

}
