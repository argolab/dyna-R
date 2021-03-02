package org.dyna.term

case class TermSize(num_pointers: Int, primitive_size: Int) {
  def +(o: TermSize) = TermSize(num_pointers + o.num_pointers, primitive_size + o.primitive_size)
}
