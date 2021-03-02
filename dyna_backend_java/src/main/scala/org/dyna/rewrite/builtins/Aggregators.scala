package org.dyna.rewrite.builtins

import org.dyna.rewrite.{Rewrite, RewriteEnvironment}
import org.dyna.term.Term


/**
 * Rewrite which manages rewrites, something of the pattern
 * aggregator(output_variable, aggregator_op, input_variable, nested_rexpr)
 */
class Aggregators extends Rewrite {

  override def rewriteFunctorName: String = "aggregator"
  override def rewriteFunctorArity: Int = 4

  override def canRewrite(term: Term)(implicit ctx: RewriteEnvironment): Boolean = ???

  override def doRewrite(term: Term)(implicit ctx: RewriteEnvironment): Term = ???
}
