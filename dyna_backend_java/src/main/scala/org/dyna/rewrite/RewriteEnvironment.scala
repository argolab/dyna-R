package org.dyna.rewrite

import org.dyna.term.Term

import scala.collection.mutable

class RewriteEnvironment (val rootTerm: Term, val parent: RewriteEnvironment=null) {

  def lookup(term: Term): List[TermContextInformation] = {
    ???
  }

  def lookup(term: Term, containedIn: String): List[TermContextInformation] = {
    ???
  }


  def construct(term: Term, parent: RewriteEnvironment=null): RewriteEnvironment = {
    val termMap = new mutable.HashMap[Term,TermContextInformation]()
    val termMapContains = new mutable.HashMap[(Term,String),TermContextInformation]()
    def walkTerm(t: Term, cameFrom: TermContextInformation): Unit = {
      val tci = new TermContextInformation(t, cameFrom)
      termMap(t) = tci
      if(cameFrom != null)
        termMapContains((t, cameFrom.term.getName)) = tci
      if(t.getName != "+") { // this needs to know which of the terms are represented for a given pattern
        // in the case this is not a disjunction we will walk through the children
        // otherwise this can be ignored

        // this needs to handle project / aggregators also
        for (i <- 0 until t.getArity) {
          t.getArgument(i) match {
            case ct: Term => walkTerm(ct, tci)
            case _ => {} // this is something like a primitive value, in which case, this is something that we can just ignroe for now
          }
        }
      }
    }

    walkTerm(term, null)

    ???
    new RewriteEnvironment(null, null)
  }

}

class TermContextInformation (val term: Term, val parent: TermContextInformation)


