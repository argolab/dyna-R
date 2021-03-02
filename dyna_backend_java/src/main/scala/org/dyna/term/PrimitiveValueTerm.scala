package org.dyna.term

abstract class PrimitiveValueTerm extends Term {
  def +(o :PrimitiveValueTerm): PrimitiveValueTerm
  def -(o :PrimitiveValueTerm): PrimitiveValueTerm
  def *(o :PrimitiveValueTerm): PrimitiveValueTerm
  def /(o :PrimitiveValueTerm): PrimitiveValueTerm

  def <(o :PrimitiveValueTerm): Boolean
  def <=(o :PrimitiveValueTerm): Boolean
  def ==(o: PrimitiveValueTerm): Boolean
  def !=(o: PrimitiveValueTerm): Boolean = !(this == o)
}

object PrimitiveValueTerm {

  def construct(o: Object): PrimitiveValueTerm = {
    o match {
      case v: Integer => PrimitiveValueTermInt(v)
      case _ => ???
//      case v: Long => {}
//      case v: Float => {}
//      case v: Double => {}
//      case v: BigInt => {}
//      case v: String => {}
    }
  }
}

case class PrimitiveValueTermInt(value: Int) extends PrimitiveValueTerm {
  override def+(o: PrimitiveValueTerm) = o match {
    case PrimitiveValueTermInt(o) => PrimitiveValueTermInt(value + o)
  }
  override def -(o: PrimitiveValueTerm) = o match {
    case PrimitiveValueTermInt(o) => PrimitiveValueTermInt(value - o)
  }
  override def *(o: PrimitiveValueTerm) = o match {
    case PrimitiveValueTermInt(o) => PrimitiveValueTermInt(value * o)
  }
  override def /(o: PrimitiveValueTerm) = o match {
    case PrimitiveValueTermInt(o) => PrimitiveValueTermInt(value / o)
  }

  override def <(o: PrimitiveValueTerm): Boolean = o match {
    case PrimitiveValueTermInt(o) => value < o
  }
  override def <=(o: PrimitiveValueTerm): Boolean = o match {
    case PrimitiveValueTermInt(o) => value <= o
  }
  override def ==(o: PrimitiveValueTerm): Boolean = o match {
    case PrimitiveValueTermInt(o) => value == o
  }

  override def signature: TermInfo = TermInfo.primitive_int

  override def getArgument(idx: Int): AnyRef = {
    assert(idx == 0)
    value.asInstanceOf[Integer]
  }
}