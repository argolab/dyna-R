package org.dyna.rewrite.builtins

import org.dyna.rewrite.Variable
import org.dyna.term.Term

class RewriteRuleCompiler {

}

object RewriteRuleCompiler {

  // would be better if this looked like our term representation
  // something like

  val rules2: Seq[(Term, Term)] = Seq(
    Term("*", Term("lessthan", Variable("A"), Variable("B")),
               Term("lessthan", Variable("B"), Variable("C"))) ->
     Term("*", Term("lessthan", Variable("A"), Variable("B")),
               Term("lessthan", Variable("B"), Variable("C")),
               Term("lessthan", Variable("A"), Variable("C")))
  )

  def compile(str: String): (Term, Term) = {
    // if this could match the given patterns, then that would provide some way in which this would
    ???
  }

  val rules: Seq[(String,String)] = Seq(
    ("(*(lessthan A B) (lessthan B C))", "(*(lessthan A B) (lessthan B C) (lessthan A B)"),

  )
}
