package org.dyna.term

class SimpleTerm(private val name: String,
                 private val arguments: Array[Term]) extends Term {

  override def getName = name

  override def getArity = arguments.length

  override def getArgument(idx: Int): AnyRef = arguments(idx)

  override lazy val signature: TermInfo = {
    // this should go through the different argumetns and determine which value
    null
  }
}
