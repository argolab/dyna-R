package org.dyna.rewrite.builtins

import org.dyna.rewrite.{Rewrite, RewriteEnvironment, VariableBound, VariableFree}
import org.dyna.term.{Multiplicity, PrimitiveValueTerm, Term}

class Plus extends Rewrite {

  override def rewriteFunctorName: String = "plus"
  override def rewriteFunctorArity: Int = 3


  override def canRewrite(term: Term)(implicit ctx: RewriteEnvironment): Boolean = {
    // this needs to determine if there is some way in which this
    false
  }

  override def doRewrite(term: Term)(implicit ctx: RewriteEnvironment): Term = {
    // this needs to construct an assignment term, which can then be processed some way
    assert(term.getName == "plus" && term.getArity == 3)

    term match {
      case Term("plus", VariableBound(_, valA: PrimitiveValueTerm), VariableBound(_, valB: PrimitiveValueTerm), VariableBound(_, valC: PrimitiveValueTerm)) => {
        // this is going to require some number value
        // this is not going to work as is as this will require matching against the different values
        // that we can have here
        if(valA + valB == valC) {
          Multiplicity(1)
        } else {
          Multiplicity(0)
        }
      }
      case Term("plus", VariableBound(_, valA :PrimitiveValueTerm), VariableBound(_, valB :PrimitiveValueTerm), VariableFree(c)) => {
        val res = valA + valB
        Term("=", c, res)
      }
      case t => t // if we can not match, then this is
    }
  }


}
