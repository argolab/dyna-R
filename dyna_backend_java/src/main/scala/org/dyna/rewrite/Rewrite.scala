package org.dyna.rewrite

import org.dyna.rewrite.Rewrite.register
import org.dyna.rewrite.builtins.{Aggregators, Lessthan, Plus}
import org.dyna.term.{PrimitiveValueTerm, Term}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * There should be rewrites which take a given pattern and then return some change
 * These are going to have to match against stuff in the environment also
 */
trait Rewrite {

  def rewriteFunctorName: String // the name of the outer functor which is matched for a given function
  def rewriteFunctorArity: Int = -1

  def doRewrite(term: Term)(implicit ctx: RewriteEnvironment): Term

  def canRewrite(term: Term)(implicit ctx: RewriteEnvironment): Boolean

}


object Rewrite {

  val rewrites = new mutable.HashMap[String,mutable.ListBuffer[Rewrite]]()

  def register(r: Rewrite): Unit = {
    if(!rewrites.contains(r.rewriteFunctorName))
      rewrites(r.rewriteFunctorName) = new ListBuffer[Rewrite]()
    rewrites(r.rewriteFunctorName).append(r)
  }

  // the classes are not going to get initalized unless there is something to force them to be loaded

  register(new Plus)
  register(new Aggregators)
  register(new Lessthan)


  def uniquifyVariables(term: Term)(implicit ctx: RewriteEnvironment): Term = {
    // this just needs to give all of the things which are projected variable names such that
    val m = new mutable.HashMap[Term, Term]()
    var var_counter = 0
    def newVariableName() = {
      var_counter += 1
      Variable("$VARIABLE_"+var_counter)
    }
    def walk(term: Term): Term = {
      term match {
        case Term("proj", Variable(x), rexpr: Term) => {
          // anytime that the variable x is found in some expression, we are going
          val prev = m.get(x)
          val new_name = newVariableName()
          m(x) = new_name
          val body = walk(rexpr)
          prev match {
            case None => m -= x
            case Some(p) => m(x) = p
          }
          Term("proj", new_name, body)
        }
        case Term("aggregator", Variable(dest), Variable(source), rexpr: Term) => {
          val prev = m.get(source)
          val new_name = newVariableName()
          m(source) = new_name
          val body = walk(rexpr)
          prev match {
            case None => m -= source
            case Some(x) => m(source) = x
          }
          Term("aggregator", dest, new_name, body)
        }
        case t => {
          // if there is some variable which is represented here, then it needs to change those variable names
          // otherwise for all of the other things, this can just have that this is going to
          // map the various values.  In the case that there is something taht
          var did_change = false
          val args = t.arguments.map({
            case Variable(v) => {
              val n = m.getOrElse(v, v) // possible for the map to contain x => x to itself, so we are just going to have this always get something
              if(n != v) n else v
            } // this is a variable name argument for this expression, so we are not going into its nested expression
            case t: Term => {
              val n = walk(t)
              if(n != t) did_change = true
              n
            }
          })
          if(did_change)
            Term.constructTerm(t.getName, args)
          else
            t // return unchanged in this case
        }
      }
    }
    walk(term)
  }

  def simplifyOnce(term: Term): Term = {
    implicit val env = new RewriteEnvironment(term, null)
    simplifyOnceRec(term)
  }

  def simplifyOnceRec(term: Term)(implicit env: RewriteEnvironment): Term = {
    // this should match against the different recursive calls
    def recurse(term: Term): Term = {
      var did_change = false
      val args = term.arguments.map({
        case Variable(v) => v
        case x: PrimitiveValueTerm => x
        case t => {
          val n = simplifyOnceRec(t)
          if (n != t) {
            did_change = true
            n
          } else {
            t
          }
        }
      })
      if(did_change) Term.constructTerm(term.getName, args) else term
    }
    // should this recurse first.  this would depend on which rewrites are possible I suppose
    rewrites.get(term.getName) match {
      case Some(rrs) => {
        for(rw <- rrs) {
          // the order in which rewrites are done depends on the kind of expression
          // if the expression is something that can be done early, then it should not match against a given expression like this
          // so there should be something that determines if there is something that would correspond
          if(rw.canRewrite(term)) {
            // if we find something that we can rewrite, then we are going to do that
            return rw.doRewrite(term)
          }
        }
        // this means that nothing was rewritten here? or nothing matched against this expression
        recurse(term)
      }
      case None => recurse(term) // then there is no rewrite for this
    }
  }

}


