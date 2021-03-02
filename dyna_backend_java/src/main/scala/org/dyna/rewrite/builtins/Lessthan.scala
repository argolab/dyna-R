package org.dyna.rewrite.builtins

import org.dyna.rewrite.{Rewrite, RewriteEnvironment, Variable, VariableBound}
import org.dyna.term.{Multiplicity, PrimitiveValueTerm, Term}

class Lessthan extends Rewrite {

  override def rewriteFunctorName: String = "lessthan"
  override def rewriteFunctorArity: Int = 2

  override def canRewrite(term: Term)(implicit ctx: RewriteEnvironment): Boolean =
    Variable.isBound(term.getArgument_Term(0)) && Variable.isBound(term.getArgument_Term(1))

  override def doRewrite(term: Term)(implicit ctx: RewriteEnvironment): Term = term match {
    case Term("lessthan", VariableBound(_, valA: PrimitiveValueTerm), VariableBound(_, valB: PrimitiveValueTerm)) =>
      if(valA < valB) Multiplicity(1) else Multiplicity(0)
    case _ => ??? // this should match the expression lessthan otherwise this is not a number, or something is not well formed
  }

  // how is this going to have a representation that works in the case of truffle
  // this would find that there are
}
