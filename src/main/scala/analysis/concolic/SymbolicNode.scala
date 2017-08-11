case class SymbolicNode(stringRepresentation: String,
                        left: Option[SymbolicNode],
                        right: Option[SymbolicNode])