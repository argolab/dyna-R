package org.dyna

import org.dyna.rewrite.{Rewrite, Variable}
import org.dyna.term.Term

object BasicTestMain {

  def main(args: Array[String]): Unit = {
    println("in the main test method")


//    val term = Term("plus", 1,2,3)
//
//    val res = Rewrite.simplifyOnce(term)
//    println(res)

    val term2 = Term("*", Term("=", Variable("X"), 5), Term("plus", Variable("X"), 2, Variable("Y")))

    val res2 = Rewrite.simplifyOnce(term2)
    println(res2)
  }

}
