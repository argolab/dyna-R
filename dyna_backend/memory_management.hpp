#include <cstdlib>
#include <cstdint>

#include "terms.hpp"


namespace dyna {

struct Term;

class TermCompactingHeap;
class TermRuntimePointer;


class TermCompactingHeap {
private:

  // used directly by the bump allocator
  uint8_t *bump_allocator_ptr;
  uint8_t *allocation_buffer_end;

  // track where the current allocation buffer starts
  uint8_t *allocation_buffer;

  // the second buffer which is copied to and swapped out in the case that we need more space
  uint8_t *second_buffer;
  uint8_t *second_buffer_end;

  friend class TermRuntimePointer;
  TermRuntimePointer *roots = nullptr;

  Term *move_to_new_heap(Term *ptr);
  Term *gc_heap_and_allocate(size_t n_bytes);

  void reallocate_second_buffer(size_t n_bytes);

public:
  TermCompactingHeap();
  ~TermCompactingHeap();

  inline Term * allocate(const TermInfo *info) {
    Term *ret = (Term*)bump_allocator_ptr;
    bump_allocator_ptr += sizeof(Term) + info->n_bytes;
    if(((intptr_t)bump_allocator_ptr) > ((intptr_t)allocation_buffer_end)) {
      ret = (Term*)gc_heap_and_allocate(info->n_bytes);
    }
    ret->info = info;
    //ret->ref_count = 1 << 28; // large number such that this is not going to cause something to allocate or deallocate this if this gets passed to python?
    return ret;
  }


  /**
   * Make a copy of the object which will now be reference counted.  This will make all of the nested values which are he different values
   */
  Term *convert_to_refcount(const Term*);


  bool is_on_heap(const Term *ptr) const {
    return (intptr_t)(allocation_buffer) <= (intptr_t)(ptr) &&
      (intptr_t)(ptr) < (intptr_t)(allocation_buffer_end);
  }


  // there might be some point where we are not trackng all of the roots, so we would want to avoid doing allocations in those cases
  // though this would require that there are some of the values
  void safe_run_gc_point(size_t n_bytes=1024*1024);

};

class TermRuntimePointer {
  // this could be a pointer to a Term which is used during rutime
private:
  TermCompactingHeap *heap;
  TermRuntimePointer *next_ptr;
  Term *ptr;
  friend class TermCompactingHeap;
public:
  TermRuntimePointer(TermCompactingHeap *heap) : heap(heap), next_ptr(heap->roots) { heap->roots = this; }
  ~TermRuntimePointer() { assert(heap->roots == this); heap->roots = next_ptr; }
  TermRuntimePointer(const TermRuntimePointer &x) : heap(x.heap), next_ptr(x.heap->roots), ptr(x.ptr) {
    heap->roots = this;
  }

  inline Term* operator->() const { return ptr; }
  inline Term* operator*() const { return ptr; }
  Term* operator=(Term *p) { ptr = p; return p; }

};


  // this does not make it possible to do the allocations from python
  // efficiently.  I suppose that we can just have that this will do the
  // allocations normally, and the copy the values into the heap such that the
  // operations can be exercised.  In the case that it is written in C++, then it would be able to identify that the expressions are



  thread_local TermCompactingHeap *active_heap;
}
