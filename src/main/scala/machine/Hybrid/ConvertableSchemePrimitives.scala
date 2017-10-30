trait ConvertableSchemePrimitives[Addr, Abs] extends SchemePrimitives[Addr, Abs] {
  def convert[OtherAbs: IsConvertableLattice](prim: Primitive[Addr, Abs], prims: SchemePrimitives[Addr, OtherAbs]): Primitive[Addr, OtherAbs]
}
