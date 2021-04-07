package org.dyna.term

import java.lang.reflect.Field

abstract class Term {

  /**
   * Return the name of the Term.  If the term is something like `foo(1,2,3)` the string "foo" will be returned
   * @return
   */
  def getName: String = signature.name

  /**
   * Return the arity of the Term.  If the term is something like `foo(1,1,1)` the value `3` will be returned
   * @return
   */
  def getArity: Int = signature.getArity

  def signature: TermInfo

  final def nameArity = (getName, getArity)

  def unwrapTerm: Term = this

  /**
   * Return one of the arguments of this term
   * @param idx
   * @return
   */
  def getArgument(idx: Int): Object
  def getArgument_Term(idx: Int): Term = getArgument(idx).asInstanceOf[Term]

  def arguments: IndexedSeq[Term]  = (0 until getArity).map(getArgument_Term(_))

  // if we are going to wrap everything in having some wrapper, then should these methods come out?
  def getArgument_int32(idx: Int): Int = getArgument(idx).asInstanceOf[Int]
  def getArgument_int64(idx: Int): Long = getArgument(idx).asInstanceOf[Long]
  def getArgument_float32(idx: Int): Float = getArgument(idx).asInstanceOf[Float]

  def builtin_eq(o: Object): Boolean = equals(o)

  override def equals(obj: Any): Boolean = {
    if(!(obj.isInstanceOf[Term])) return false
    val t = obj.asInstanceOf[Term]
    if(t.getName != getName || getArity != t.getArity) return false

    // in the case that this ifnds

    for(i <- 0 until getArity)
      if(getArgument(i) != t.getArgument(i)) return false
    true
  }

  // python uses underscore named typically, so add these so we can still have methods
  // which return the right thing
//  final def get_name = getName
//  final def get_arity = getArity
//  final def get_argument(idx: Int) = getArgument(idx)
  // jnius does not expose java's hash methods.  This seems like something that it should be doing
  // there is the ability to add methods to the class protocol which would then expose stuff
//  final def __hash__ = hashCode()
//  final def __eq__(o: Object) = equals(o)

  override def toString: String = {
    // in the case that the nested term is very big, this should maybe consider somehow having some indent?
    val r = new StringBuilder
    r.append(getName)
    if(getArity > 0) {
      r.append("(")
      for ((a, i) <- arguments.zipWithIndex) {
        if (i != 0) r.append(", ")
        r.append(a.toString)
      }
      r.append(")")
    }
    r.toString()
  }

}


abstract class PrimitiveTerm extends Term {
  // this should just return some int/value
}

object Term {

  def constructTerm(name: String, args: Array[Term]): Term = {
    new SimpleTerm(name, args)
  }

  def constructTerm(name: String, args: Seq[Term]): Term = {
    new SimpleTerm(name, args.toArray)
  }


  def apply(name: String, args: Any*): Term = {
    constructTerm(name, args.map({
      case t: Term => t // if this is already a term, then we just keep it, otherwise
      case o => PrimitiveValueTerm.construct(o)
    }).toArray)
  }

  // how does this construct a flattened term.  In the case that there is something which would

  def unapplySeq(t: Term): Option[Seq[Any]] = {
    Some(Seq(t.getName) ++ (0 until t.getArity).map(t.getArgument))
  }
}


object Multiplicity {

  val multiplicity_name = "$MULTIPLICITY"

  def apply(i: Int): Term= {
    Term(multiplicity_name, Integer.valueOf(i))
  }
}

