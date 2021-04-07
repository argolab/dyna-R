package org.dyna.term

class SimpleTerm(private val name: String,
                 private val argumentsA: Array[Term]) extends Term {

  override def getName = name

  override def getArity: Int = argumentsA.length

  override def getArgument(idx: Int): AnyRef = argumentsA(idx)

  override def arguments: IndexedSeq[Term] = argumentsA

  override lazy val signature: TermInfo = {
    // this should go through the different argumetns and determine which value
    null
  }
}
