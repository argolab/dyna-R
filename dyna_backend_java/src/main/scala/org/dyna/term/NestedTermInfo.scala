package org.dyna.term

import java.lang.reflect.Field

class NestedTermInfo(name: String, arguments: List[NestedTermInfo],
                     val isPrimitive: Boolean,
                     val field: Field,
                    ) extends TermInfo(name, arguments) {

  def constructWrapper(obj: Term): Term = ??? // this should reutrn some wrapping term which

}
