package org.dyna

class SimpleTerm(override val name: String,
                 private val arguments: Array[Term]) extends Term {

  override def arity = arguments.length

  override def getArgument(idx: Int): AnyRef = arguments(idx)

  override lazy val signature: TermInfo = {
    // this should go through the different argumetns and determine which value
    null
  }
}
