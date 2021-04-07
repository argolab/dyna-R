package org.dyna.term

import java.lang.reflect.Field
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TermInfo(val name: String, val arguments: List[NestedTermInfo]) {
  def primitive: Boolean = false

  def pointer: Boolean = false

  def size: TermSize = arguments.map(_.size).reduce(_ + _)

  def getName: String = name

  def getArity: Int = arguments.length


  /**
   * This should return the field access which will directly access the relevant element
   * In the case that there is something
   *
   * @return
   */
  def getField: Field = null

  def getArgument(idx: Int): TermInfo = ??? //arguments(idx).info

  // this could be something which is used to reference what field is used to hold a given value
  // though in the case that something is nested many levels down, then how would that work
  // in the case
  private val field: Field = null
}

object TermInfo {

  val pointer_info = new TermInfo("primitive_pointer", null) {
    override def pointer: Boolean = true
    override def size = TermSize(1, 0)
    override def getArity: Int = 1
  }

  val primitive_int = new TermInfo("primitiv_int32", null) {
    override def primitive: Boolean = true
    override def size = TermSize(0, 4)
    override def getArity: Int = 1
  }

  val primitive_int64 = new TermInfo("primitive_int64", null) {
    override def primitive: Boolean = true
    override def size = TermSize(0, 8)
    override def getArity: Int = 1
  }

  val primitive_float32 = new TermInfo("primitive_float32", null) {
    override def primitive: Boolean = true
    override def size: TermSize = TermSize(0, 4)
    override def getArity: Int = 1
  }

  val primitive_string = new TermInfo("primitive_string", null) {
    override def size = TermSize(1, 0)
    override def getArity: Int = 1
  }

  def getTermInfo(v: Object): TermInfo = {
    if (v.isInstanceOf[Term]) v.asInstanceOf[Term].signature
    else if (v.isInstanceOf[Int]) primitive_int
    else if (v.isInstanceOf[Long]) primitive_int64
    else if (v.isInstanceOf[Float]) primitive_float32
    else if (v.isInstanceOf[String]) primitive_string
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

  /**
   * If we are going to be constructing classes which back these object, then we should ensure that these also
   */
  val constructedTermInfos = new mutable.HashMap[(String, Int), mutable.ListBuffer[TermInfo]]() {
    override def default(x: (String,Int)) = new ListBuffer[TermInfo]()
  }
}