#ifndef _DYNA_TERMS
#define _DYNA_TERMS

#include <string>
#include <vector>
#include <assert.h>
#include <stdlib.h>
#include <iostream>
#include <typeinfo>

namespace dyna {

// dyna primitive types
typedef int32_t int_d;
typedef float float_d;
typedef bool bool_d;

struct TermInfo;
struct NestedTermInfo;
struct Term;

struct TermContainer;
struct Term;


struct NestedTermInfo {
  TermInfo *term;   // the info pointer for this object itself
  uint offset:31;   // what is the byte offset (from value) for this object
  uint embedded : 1= 0;  // if the values are embedded in this object, or if they are a pointer elsewhere
};

struct TermInfo {
  uint32_t n_bytes;  // the primitive size of this object

  // for basic primitive quote like expression &foo(1,2,3).
  std::string name;
  uint32_t arity;
  std::vector<NestedTermInfo> args;

  bool unique_info = false; // if this should also get deleted when the Term object is deleted.  If we are reusing this, then this should be false

  // there should be some information about what is being held
  bool is_ptr = false;
  std::type_info const *held_type = nullptr;
  void (*deallocator)(void*) = nullptr; // if there is something else in the
  // container, then we should hold pointers
  // to the relevant methods.  The most
  // important is the ability to deallocate
  // the object (given that it might
  // reference other external structures)

  // there needs to be some operation table, that we can match against.  If
  // there are types in the dispatch, then that makes /some/ things a bit easier, but then there are


  TermInfo (std::string name, uint32_t n_bytes) : name(name), n_bytes(n_bytes) {}
  TermInfo() {}

  template<typename T>
  static TermInfo* create (bool is_ptr=false) {
    TermInfo *ret = new TermInfo();
    ret->held_type = &typeid(T);
    if(is_ptr) {
      // if we hold a pointer to the object instead of the object itself
      ret->n_bytes = sizeof(T*);
      ret->deallocator = [](void *ptr) { delete (T*)ptr; };
    } else {
      ret->n_bytes = sizeof(T);
      ret->deallocator = [](void *ptr) { ((T*)ptr)->~T(); }; // make it so we can call the deconstructor
    }
    ret->is_ptr = is_ptr;
    return ret;
  }
};


extern const TermInfo StaticIntTag;
extern const TermInfo StaticFloatTag;
extern const TermInfo StaticBoolTag;

extern const TermInfo StaticInt64Tag;
extern const TermInfo StaticFloat64Tag;






struct Term {
  const TermInfo *info;
  mutable uint32_t ref_count = 0;//: 31;
  //uint shared_between_threads : 1; // if we are sharing between threads, then it
  // would want to use atomics, or just not
  // perform ref counting, and do some GC pass
  // later?  We could instead keep these
  // objects on some linked list (python
  // style), or allocate shared objects into
  // some managed pool for between thread
  // operations?

  uint8_t values[];


  ~Term() {
    if(info && info->deallocator) { info->deallocator((void*)values); }
  }

  Term() { info = nullptr; ref_count = 0; /*shared_between_threads = 0;*/ }

  void incr_ref() const {
    ref_count++;
  }

  void decr_ref() const {
    // these are not thread save, so would like to have some way to set that an object is in a shared heap
    // or some thread local heap or something.  Then we can handle the two cases
    ref_count--;
    if(ref_count == 0) {
      delete const_cast<Term*>(this);
    }
  }

  // TermContainer &access(int i) {
  //   // return some container which represents what information has been stored in this object
  //   return
  // }

  void unpack(uint i, TermContainer &c) const {
    // unpack the element at position i into container c
    // basically get what type this is, find the offset position, and then construct some wrapper for that element
  }

  static Term *createInt(int_d i) {
    Term *ret = (Term*)malloc(sizeof(Term)+sizeof(int_d));
    ret->info = &StaticIntTag;
    *((int_d*)ret->values) = i;
    return ret;
  }
  static Term *createFloat(float_d f) {
    Term *ret = (Term*)malloc(sizeof(Term)+sizeof(float_d));
    ret->info = &StaticFloatTag;
    *((float_d*)ret->values) = f;
    return ret;
  }
  static Term *createBool(bool_d b) {
    // this should return some static term objects, so we are not creating new instances all of the time
    assert(false);
    return nullptr;
  }
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

  inline bool is_ptr() const { return ((uintptr_t)ptr) & 0x1 == 0; }

  void set_pointer(const Term *ptr) {
    assert(( ((uintptr_t)ptr) & 0x1) == 0);
    ptr->incr_ref();
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
