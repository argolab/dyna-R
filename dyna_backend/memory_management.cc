#include <stdlib.h>

#include "memory_management.hpp"


namespace dyna {

TermCompactingHeap::TermCompactingHeap() {
  allocation_buffer = (uint8_t*)malloc(1024*1024);
  allocation_buffer_end = allocation_buffer + 1024*1024;
  second_buffer = (uint8_t*)malloc(1024*1024);
  second_buffer_end = second_buffer + 1024*1024;
}

TermCompactingHeap::~TermCompactingHeap() {
  free(allocation_buffer);
  free(second_buffer);
}

void TermCompactingHeap::reallocate_second_buffer(size_t n_bytes) {
  free(second_buffer);
  second_buffer = (uint8_t*)malloc(n_bytes);
  second_buffer_end = second_buffer + n_bytes;
}

Term *TermCompactingHeap::move_to_new_heap(Term *ptr) {
  // if this pointer is not on the old heap, then return unchanged
  if(!((intptr_t)(second_buffer) <= (intptr_t)(ptr) && (intptr_t)(ptr) < (intptr_t)(second_buffer_end)))
    return ptr;

  // if the object has already be moved, then we should just return the forwarding pointer
  // if the first address in the object points to the new heap, then that can indicate that this is a forwarding pointer
  Term *forward_ptr = *(Term**)(ptr);
  if((intptr_t)(allocation_buffer) <= (intptr_t)(forward_ptr) && (intptr_t)(forward_ptr) < (intptr_t)(allocation_buffer_end))
    return forward_ptr;

  // then the object needs to be moved
  const TermInfo *info = ptr->info;

  // then this needs to allocate the new space, and the start coping the values over
  Term *new_address = (Term*)(bump_allocator_ptr);
  bump_allocator_ptr += sizeof(Term) + info->n_bytes;
  assert((intptr_t)(bump_allocator_ptr) < (intptr_t)(allocation_buffer_end)); // otherwise we have overrun the buffer

  memcpy(new_address, ptr, sizeof(Term) + info->n_bytes); // copy the primitive values

  // set the forwarding address
  *(Term**)(ptr) = new_address;

  info->walk_all_pointers(info, ptr, [](void *ptr, void *transparent) -> void* {
    TermCompactingHeap *self = static_cast<TermCompactingHeap*>(transparent);
    return self->move_to_new_heap(static_cast<Term*>(ptr));
  }, this);

}

Term *gc_heap_and_allocate(size_t n_bytes) {
  // called in the case that we need to run the gc because we have run out of space
  // if the second heap
  if((static_cast<intptr_t>(second_buffer_end) - static_cast<intptr_t>(second_buffer)) <
     (static_cast<intptr_t>(allocation_buffer_end) - static_cast<intptr_t>(allocation_buffer))) {
    // then the second buffer is smaller than the primary, so we are going to allocate a new buffer for this
    reallocate_second_buffer(static_cast<intptr_t>(allocation_buffer_end) - static_cast<intptr_t>(allocation_buffer));
  }

  swap(allocation_buffer, second_buffer);
  swap(allocation_buffer_end, second_buffer_end);

  bump_allocator_ptr = allocation_buffer;

  // go through all of the root pointers and copy the values to the new values
  TermRuntimePointer *p = roots;
  while(p != nullptr) {
    p->ptr = move_to_new_heap(p->ptr);
    p = p->next_ptr;
  }

  // if the amount of space which is allocated due to copying is above some threshold, then this should increase the amount of space
  if((static_cast<intptr_t>(allocation_buffer_end) - static_cast<intptr_t>(bump_allocator_ptr) + n_bytes) >
     .3* (static_cast<intptr_t>(allocation_buffer_end) - static_cast<intptr_t>(allocation_buffer))) {
    // double the size of the second buffer
    size_t new_size = 2*(static_cast<intptr_t>(allocation_buffer_end) - static_cast<intptr_t>(allocation_buffer));
    while(new_size < n_bytes*2) new_size *= 2;
    reallocate_second_buffer(new_size);
  }

  if(static_cast<intptr_t>(bump_allocator_ptr + n_bytes) > static_cast<intptr_t>(allocation_buffer_end)) {
    // then there is not enough space still, so we are going to reallocate the space
    return gc_heap_and_allocate(n_bytes);
  }

  // return the pointer to the new space
  Term *ret = bump_allocator_ptr;
  bump_allocator_ptr += n_bytes;
  return ret;
}

const Term *convert_to_refcount(const Term *term) {
  if(!is_on_heap(term)) return term;


  assert(false); // todo handle
}

}
