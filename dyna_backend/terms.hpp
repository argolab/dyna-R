#ifndef _DYNA_TERMS
#define _DYNA_TERMS

#include <string>
#include <vector>
#include <assert.h>
#include <stdlib.h>
#include <iostream>
#include <typeinfo>

#include <pybind11/pybind11.h>
namespace py = pybind11;

namespace dyna {

// dyna primitive types
typedef int32_t int_d;
typedef float float_d;
typedef bool bool_d;

// 32 bits should be enough for the hash code, though with more bits, then it may be better for hashing stuff
typedef uint32_t hashcode_T;

// a list in dyna will be '.'(1, '.'(2, nil()))
// though this can become an object which is flattened in memory, so it might be ok still
#define DYNA_LIST_EMPTY_NAME "nil"
#define DYNA_LIST_CONS_NAME "."

struct TermInfo;
struct NestedTermInfo;
struct Term;

struct TermContainer;

struct TermInfoPointer {
  static TermInfo *term_info_base_pointer;  // these could be allocated allocated into some array, and then later filled in as kinds of terms are created
  // though if this is also be used to track the program states, the nit might not be a good idea to have this represented
  // uint term_info_id : 31;
  // uint gc_mark_and_sweep : 1;
  const uint32_t term_info_id;
  // dyna::TermInfo *operator*() const { return term_info_base_pointer + term_info_id; }
  // dyna::TermInfo *operator->() const { return term_info_base_pointer + term_info_id; }

};


struct NestedTermInfo {
  const TermInfo *term;   // the info pointer for this object itself
  uint offset:31;         // what is the byte offset (from value) for this object
  uint embedded : 1= 0;   // if the values are embedded in this object, or if they are a pointer elsewhere

  //uint is_offset_from_base; // if this is nested many levels down.  We might want to mark that this is not some nested
                              // value but that this is offset from the root pointer?  Though how would that work in the case
                              // that there are differences with which of the values it encounters.  If there is something that
                              // it encounters in the expression.  The root pointer might not always be avaliable also (at least how this is implemented atm)
  // we could just allow the offset to also have a negative number.  In which case it would offset from its current nested location.  Though that might also be somewhat annoying for it to handle which of the expressions are getting rewritten.
  // if there are values which correspond with what of the values are mapped
  // if there was a variable reference, the it would find that it would be referencing back to the point where the cell is located.  As more values are added into the program, it would find that there is something which needs to get mapped.


  // I suppose that if embedded is set to 1, and term was set to nullptr, then that could be considered a generic thing
  // there could also just be some specific pointer type, and then we can just assume that everything is embedded
  // from that point, it would just be tracking the struct info.  If everything is embedded that makes stuff easier
};

std::string TermInfo_defaultToString(const TermInfo *info, const void *address);

struct TermInfo {
  uint32_t n_bytes;  // the primitive size of this object

  // for basic primitive quote like expression &foo(1,2,3).
  std::string name;
  uint32_t arity;
  std::vector<NestedTermInfo> args;

  bool unique_info = false; // if this should also get deleted when the Term object is deleted.  If we are reusing this, then this should be false

  // there should be some information about what is being held
  bool is_ptr = false; // XXX: what is going to be the point of this????
  bool is_non_trivial = false; // meaning that custom_deallocator or custom_copy is set for this or something nested inside inside of this.  This would require scanning through the entire structure to find the call the relevant methods
  std::type_info const *held_type = nullptr;

  // container, then we should hold pointers
  // to the relevant methods.  The most
  // important is the ability to deallocate
  // the object (given that it might
  // reference other external structures)

  // there needs to be some operation table, that we can match against.  If
  // there are types in the dispatch, then that makes /some/ things a bit easier, but then there are


  // TermInfo (std::string name, uint32_t n_bytes) : name(name), n_bytes(n_bytes) {}
  // TermInfo() {}

  template<typename T>
  static TermInfo* create (bool is_ptr=false) {
    TermInfo *ret = new TermInfo();
    ret->held_type = &typeid(T);
    if(is_ptr) {
      // if we hold a pointer to the object instead of the object itself
      ret->n_bytes = sizeof(T*);
      ret->custom_deallocator = [](void *ptr) { delete (T*)ptr; };
      // the store from / to python methods oculd just do casting?
      // though those casting methods would go through pybind so how would that work here?
    } else {
      ret->n_bytes = sizeof(T);
      ret->custom_deallocator = [](void *ptr) { ((T*)ptr)->~T(); }; // make it so we can call the deconstructor
    }
    ret->is_ptr = is_ptr;
    return ret;
  }

  Term *allocate() const;
  void deallocate(Term*) const;


  // the annoying thing about this is that it closely ties the implementation to python

  // doing this would allow it to be more extensiable with having more types that can be stored
  // though the amount of space that would be required may change.  The TermInfo which is represented
  // for each of these values
  void (*store_from_python)(const TermInfo *info, void *address, py::handle *obj)=nullptr;
  py::object (*cast_to_python)(const TermInfo *info, const void *address)=nullptr;
  std::string (*to_string)(const TermInfo *info, const void *address)=&TermInfo_defaultToString;

  void (*custom_deallocator)(const TermInfo*, void*) = nullptr; // if there is something that needs special handling
  //void (*custom_copy)(const TermInfo*, const void *source, void *dest) = nullptr;

  hashcode_T (*get_hashcode)(const TermInfo *info, const void *self) = nullptr;

  //
  void (*walk_all_pointers)(const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent), void *transparent) = nullptr;

  // the operations which are for some of the values
  //void (*visit_nested_rexprs)(const TermInfo *info, const void*, const TermVisitor*, TermNestedContext*) = nullptr;
  //void (*rename_vars)(const TermInfo *info, const void*)  // the renamming of variables does not pattern match which of the expressions would have for some of the values
  // in the case that there are differences with which of

  //TermInfo* (*rename_vars)(const TermInfo *info, const void *)=nullptr; // there should only have a single value in the case that the variables are renammed with different values.



};


extern const TermInfo StaticIntTag;
extern const TermInfo StaticFloatTag;
extern const TermInfo StaticBoolTag;

extern const TermInfo StaticInt64Tag;
extern const TermInfo StaticFloat64Tag;



struct Term {
  const TermInfo *info;
  mutable uint32_t ref_count = 0;//: 31;
  mutable hashcode_T  hashcache = 0;
  //uint shared_between_threads : 1; // if we are sharing between threads, then it
  // would want to use atomics, or just not
  // perform ref counting, and do some GC pass
  // later?  We could instead keep these
  // objects on some linked list (python
  // style), or allocate shared objects into
  // some managed pool for between thread
  // operations?

  uint8_t values[];



protected: // there might be a downstream item which is allocated that needs to call the deallocator....

  // this should never be allocated via new/delete.  This is going to have to be mallocated and freeded as there are additional fields
  // at the end of this object
  Term() { info = nullptr; ref_count = 0; /*shared_between_threads = 0;*/ }

  ~Term() {
    // this should never call the delete method on this, as it shold instead use info->deallocate(this) which can handle nested values properly
    // asm("int3");
    // __builtin_unreachable();
    //if(info && info->deallocator) { info->deallocator((void*)values); }
  }

public:

  void incr_ref() const {
    ref_count++;
  }

  void decr_ref() const {
    // these are not thread save, so would like to have some way to set that an object is in a shared heap
    // or some thread local heap or something.  Then we can handle the two cases
    ref_count--;
    if(ref_count == 0) {
      info->deallocate(const_cast<Term*>(this));
    }
  }



  // TermContainer &access(int i) {
  //   // return some container which represents what information has been stored in this object
  //   return
  // }

  // void unpack(uint i, TermContainer &c) const {
  //   // unpack the element at position i into container c
  //   // basically get what type this is, find the offset position, and then construct some wrapper for that element
  // }

  // static Term *createInt(int_d i) {
  //   Term *ret = (Term*)malloc(sizeof(Term)+sizeof(int_d));
  //   ret->info = &StaticIntTag;
  //   *((int_d*)ret->values) = i;
  //   return ret;
  // }
  // static Term *createFloat(float_d f) {
  //   Term *ret = (Term*)malloc(sizeof(Term)+sizeof(float_d));
  //   ret->info = &StaticFloatTag;
  //   *((float_d*)ret->values) = f;
  //   return ret;
  // }
  // static Term *createBool(bool_d b) {
  //   // this should return some static term objects, so we are not creating new instances all of the time
  //   assert(false);
  //   return nullptr;
  // }

  operator std::string() const;

  bool operator==(const Term&) const;

  inline hashcode_T hash() const { if(hashcache == 0) compute_hash(); return hashcache; }
  void compute_hash() const;
};


struct TermContainer {
  // this is the object that should be used for holding a pointer to a term.
  // does automatic tagging of primitives and manages the reference counting
  enum TermVTag {
    T_ptr = 0,
    T_int = 1,
    T_float = 3,
    T_bool = 5,
    T_nonground = 7  // for the case that this represents a non-ground value, that we want to pass to something else that is performing a lookup
  };
  union {
    Term *ptr;  // the last bits should be 0.  Otherwise we are in the tag case
    struct {
      union { // this should be 4 bytes
        int_d int_v;
        float_d float_v;
        bool_d bool_v;
      };
      uint _gap : 29;
      uint tag  :  3;
    };
  };
  TermContainer() { ptr = nullptr; }

  TermContainer(int_d i) { int_v = i; tag = T_int; }
  TermContainer(float_d f) { float_v = f; tag = T_float; }
  TermContainer(bool_d b) { bool_v = b; tag = T_bool; }

  ~TermContainer() { if(is_ptr()) { ptr->decr_ref(); } }

  inline bool is_ptr() const { return ptr != nullptr && (((uintptr_t)ptr) & 0x1) == 0; }

  void set_pointer(const Term *ptr) {
    assert(( ((uintptr_t)ptr) & 0x1) == 0);
    ptr->incr_ref();
    if(this->is_ptr()) this->ptr->decr_ref();
    this->ptr = const_cast<Term*>(ptr);
    assert(is_ptr());
  }
  Term *get_ptr() const {
    assert(is_ptr());
    return ptr;
  }

  const TermContainer &operator= (const TermContainer &other) {
    // allow assignments between two operators
    if(other.is_ptr()) {
      if(other.ptr == nullptr) {
        if(is_ptr()) ptr->decr_ref();
        ptr = nullptr;
      } else {
        other.ptr->incr_ref();  // increase the ref before we decrease the ref in the case that these are the same objects
        if(is_ptr()) ptr->decr_ref();
        ptr = other.ptr;
      }
    } else {
      if(is_ptr()) ptr->decr_ref();
      // then this is a simple value, and we should be able to just assign the value
      ptr = other.ptr; // just copy all of the data
    }
    return *this;
  }

  bool atomic_set(const TermContainer &previousValue, const TermContainer &newValue) {
    // if this is threaded code, then this should be a compare and swap
    assert(false);
  }

  bool unify_ground(TermContainer &other) {
    // this is something that
    assert(false); // this is something that needs to check that the values are the same
  }

};

}


#endif
