package org.dyna

case class TermSize(num_pointers: Int, primitive_size: Int) {
  def +(o: TermSize) = TermSize(num_pointers + o.num_pointers, primitive_size + o.primitive_size)
}

class NestedTermInfo(val info: TermInfo, val offset: TermSize, val primitive: Boolean=false) {

}

class TermInfo(val name: String, val arguments: List[NestedTermInfo]) {
  def primitive: Boolean = false
  def pointer: Boolean = false
  def size: TermSize = arguments.map(_.info.size).reduce(_+_)

  // this could be something which is used to reference what field is used to hold a given value
  // though in the case that something is nested many levels down, then how would that work
  // in the case
  val field: java.lang.reflect.Field = null
}

object TermInfo {

  val pointer_info = new TermInfo("primitive_pointer", null) {
    override def pointer: Boolean = true
    override def size = TermSize(1, 0)
  }

  val primitive_int = new TermInfo("primitiv_int32", null) {
    override def primitive: Boolean = true
    override def size = TermSize(0, 4)
  }

  val primitive_int64 = new TermInfo("primitive_int64", null) {
    override def primitive: Boolean = true
    override def size = TermSize(0, 8)
  }

  val primitive_float32 = new TermInfo("primitive_float32", null) {
    override def primitive: Boolean = true
    override def size: TermSize = TermSize(0, 4)
  }

  val primitive_string = new TermInfo ("primitive_string", null) {
    override def size = TermSize(1, 0)
  }

  def getTermInfo(v :Object): TermInfo = {
    if(v.isInstanceOf[Term]) v.asInstanceOf[Term].signature
    else if(v.isInstanceOf[Int]) primitive_int
    else if(v.isInstanceOf[Long]) primitive_int64
    else if(v.isInstanceOf[Float]) primitive_float32
    else if(v.isInstanceOf[String]) primitive_string
    else {
      // there needs to be other values which are contained for the values which are represented here
      // in the case that there are differences values which would
      assert(false)
      null
    }
  }

  def constructTermInfoForObject(cls: java.lang.Class[_ <: Term]): TermInfo = {
    // this should go through the object and find which of the fields are present, in the case
    null
  }
}