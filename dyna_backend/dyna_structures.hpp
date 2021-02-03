#ifndef _DYNA_STRUCTUERS
#define _DYNA_STRUCTUERS

namespace dyna {

  /**
   * Term is the term class.  The first field is the pointer to TermInfo which
   * describes how the term is layout (as well as the size of the term class).
   *
   * The size of the term class is not the size of the object itself (i.e. don't
   * both doing sizeof(Term)), but rather term->info->n_bytes
   *
   * Do not use `new Term` or `delete Term` but rather `TermInfo->allocate()` and `term->info->deallocate(this)`
   */
  struct Term;

  /**
   * TermContainer is the pointer class for holding pointers to the term type.
   */
  struct TermContainer;

  /**
   * TermInfo is the Term's "vtable" and contains other meta data for a given term.
   */
  struct TermInfo;
  struct NestedTermInfo;


  struct RewriteContext;

}

#endif
