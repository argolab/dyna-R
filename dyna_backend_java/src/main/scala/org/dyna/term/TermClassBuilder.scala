package org.dyna.term

import javassist.{ClassPool, CtClass}

object TermClassBuilder {

  private var term_counter: Long = 0

  def buildTermClass(info: TermInfo): Class[_ <: Term] = {
    val cls: CtClass = ClassPool.getDefault.makeClass(s"org.dyna.Generated_term_${term_counter}")
    term_counter += 1

    // this needs to add fields to the class, and then construct a new instance of this such that it would return

    var field_counter = 0

    def add_fields(info: TermInfo): Unit = {
      for ((arg, i) <- info.arguments.zipWithIndex) {
        // if the field is something that is not a primitive value, then this should either
        // decided that it is going to reference as a pointer, or this is going to

        val s = s"public int field_${field_counter}"
        field_counter += 1
      }
    }

    // is there some reason that we should set final, this would require using more stuff
    // to clear the final such that we can override this field, and then I guess set it back to being final
    // the JVM might be able to cache some of the information in the case that we mark it final
    val s = "static private org.dyna.TermInfo static_object_terminfo = null;"


    add_fields(info)

    // the different get Argument methods need to be generated, as well as we need to construct a new info
    // object which represents which values are for a particular value.
    // how

    val jclass = cls.toClass()

    // we need to construct new TermInfo object which contains the information about how this is nested
    // such that we can identify the different fields in which things are stored.


    val term_info_field = jclass.getField("static_object_terminfo")
    term_info_field.set(null, info)


    null
  }

}
