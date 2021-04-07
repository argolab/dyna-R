package org.dyna.rewrite

import org.dyna.term.Term

object Variable {
  final val variable_name = "$VARIABLE"

//  def unapply(o: Object)(implicit ctx: RewriteEnvironment): Option[Term] = {
//    if(o.isInstanceOf[Term]) unapply(o.asInstanceOf[Term])
//    else None
//  }

  def unapply(t: Term)(implicit ctx: RewriteEnvironment): Option[Term] = {
    if(t.getName == variable_name && t.getArity == 1) Some(t) else None
  }

  def apply(name: String): Term = {
    Term(variable_name, Term(name))
  }

  def isBound(name: Term)(implicit ctx: RewriteEnvironment): Boolean = {
    if(isVariable(name)) {
      // this needs to lookup if the variable is bound in the runtime environment
      val possible_assignments = ctx.lookup(name.getArgument_Term(0), "=")
      ???
      false
    } else {
      true
    }
  }

  def getValue(name: Term)(implicit ctx: RewriteEnvironment): Object = { // this should just come back as a term
    if(isVariable(name)) {
      val bindings = ctx.lookup(name).filter(c => {
        val p = c.parent
        p != null && p.term.nameArity == ("=", 2) && !Variable.isVariable(p.term.getArgument_Term(1))
      })
      if(bindings.isEmpty) null
      else {
        // this is going to return the second argument to this term
        // but this is something that we are going to want to push into some sort of general frame object eventually
        bindings.head.parent.term.getArgument(1)
      }
    } else {
      name // the term itself is the value, so we are just going to return that
    }
  }

  def setValue(name: Term, value: Object)(implicit ctx: RewriteEnvironment): Term = {
    Term("=", name, value)
  }

  def isVariable(name: Term)(implicit ctx: RewriteEnvironment): Boolean = {
    name.getName == variable_name && name.getArity == 1
  }


}

object VariableBound {
  def unapply(t: Term)(implicit ctx: RewriteEnvironment): Option[(Term, Object)] = {
    if(Variable.isBound(t)) {
      Some((t, Variable.getValue(t)))
    } else None
  }
  // this should also take some object, but this is going to want for there
}

object VariableFree {
  def unapply(t: Term)(implicit ctx: RewriteEnvironment): Option[Term] = {
    if(!Variable.isBound(t)) Some(t) else None
  }
}