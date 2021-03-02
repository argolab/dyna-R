package org.dyna.term

class WrappedTerm (override val signature : TermInfo,
                   private val wrapped: Term) extends Term {

  override def getArgument(idx: Int): AnyRef = wrapped.getArgument(idx)

  override def unwrapTerm: Term = wrapped
}
