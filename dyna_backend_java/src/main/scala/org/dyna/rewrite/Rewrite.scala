package org.dyna.rewrite

import org.dyna.rewrite.Rewrite.register
import org.dyna.rewrite.builtins.{Aggregators, Plus}
import org.dyna.term.Term

import scala.collection.mutable

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
    rewrites(r.rewriteFunctorName) += r
  }

  // the classes are not going to get initalized unless there is something to force them to be loaded

  register(new Plus)
  register(new Aggregators)


  def uniquifyVariables(term: Term): Term = {
    // this just needs to give all of the things which are projected variable names such that
    val m = new mutable.HashMap[Term, Term]()
    def walk(term: Term): Term = {
      term match {
        case Term("proj", Variable(x), rexpr) => {

        }
        case Term("aggregator", Variable(dest), Variable(source), rexpr) => {

        }
        case t => {
          // if there is some variable which is represented here, then it needs to change those variable names
          // otherwise for all of the other things, this can just have that this is going to
          // map the various values.  In the case that there is something taht
          var did_change = false

        }
      }
    }
  }

  def simplifyOnce(term: Term): Term = {
    term
  }

}


