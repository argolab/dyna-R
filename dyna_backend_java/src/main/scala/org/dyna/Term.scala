package org.dyna

abstract class Term {

  def name: String

  def arity: Int

  def getArgument(idx: Int): Object

  def signature: TermInfo

  def unwrapTerm: Term = this
}

class WrappedTerm (val info: TermInfo, val obj: Term) extends Term {
  override def name = info.name
  override def arity = info.arguments.length
  override def getArgument(idx: Int): AnyRef = {
    // if this is a primitive value, then this can just return the object, otherwise
    // we are going to have to return something that is another wrapped Term.
  }

  def unwrapTerm = {
    // this is going to have to construct a new Term which does not have the excess information contained
    // in the case that there is something
    null
  }
}

object Term {

  def constructTerm(name: String, args: Array[Term]): Term = {
    new SimpleTerm(name, args)
  }

  // how does this construct a flattened term.  In the case that there is something which would


}


