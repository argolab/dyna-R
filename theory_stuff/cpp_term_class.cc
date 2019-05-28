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
  uint embedded = 0;  // if the values are embedded in this object, or if they are a pointer elsewhere
};

struct TermInfo {
  uint32_t n_bytes;  // the primitive size of this object

  // for basic primitive quote like expression &foo(1,2,3).
  std::string name;
  uint32_t arity;
  std::vector<NestedTermInfo> args;

  bool unique_info = false; // if this should also get deleted when the Term object is deleted.  If we are reusing this, then this should be false

  // there should be some information about what is being held
  bool is_ptr;
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
    return ret;
  }
};

// this might not be required?? If we are just having the object information?
  static TermInfo *asdfasdf = TermInfo::create<int>();
static const TermInfo StaticIntTag("primitive_int", 4);
static const TermInfo StaticFloatTag("primitive_float", 4);
static const TermInfo StaticBoolTag("primitive_bool", 1);

static const TermInfo StaticInt64Tag("primitive_int64", 8);
static const TermInfo StaticFloat64Tag("primitive_float64", 8);



struct Term {
  const TermInfo *info;
  uint32_t ref_count : 31;
  uint shared_between_threads : 1; // if we are sharing between threads, then it
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

  void incr_ref() {
    ref_count++;
  }

  void decr_ref() {
    // these are not thread save, so would like to have some way to set that an object is in a shared heap
    // or some thread local heap or something.  Then we can handle the two cases
    ref_count--;
    if(ref_count == 0) {
      delete this;
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

  inline bool is_ptr() const { return (uintptr_t)ptr & 0x1 == 0; }

  void set_pointer(Term *ptr) {
    assert(( (uintptr_t)ptr & 0x1) == 0);
    ptr->incr_ref();
    this->ptr = ptr;
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
};

static_assert(sizeof(TermContainer) == sizeof(void*));

template<typename Visitor>
auto visitOp(Visitor visitor, const TermContainer &a) {
  if(a.is_ptr()) {
    return visitor(a.ptr);
  } else if(a.tag == TermContainer::T_int) {
    return visitor(a.int_v);
  } else if(a.tag == TermContainer::T_float) {
    return visitor(a.float_v);
  } else if(a.tag == TermContainer::T_bool) {
    return visitor(a.bool_v);
  }
}

template<typename Visitor>
auto visitBinaryOp(Visitor visitor, const TermContainer &a, const TermContainer &b) {
  if(a.tag == TermContainer::T_int) {
    if(b.tag == TermContainer::T_int) {
      return visitor(a.int_v, b.int_v);
    } else if(b.tag == TermContainer::T_float) {
      return visitor(a.int_v, b.float_v);
    } else if(b.tag == TermContainer::T_bool) {
      return visitor(a.int_v, b.bool_v);
    } else {
      // this should be some type failure
      assert(false);
    }
  } else if(a.tag == TermContainer::T_float) {
    if(b.tag == TermContainer::T_float) {
      return visitor(a.float_v, b.float_v);
    } else if(b.tag == TermContainer::T_int) {
      return visitor(a.float_v, b.int_v);
    } else if(b.tag == TermContainer::T_bool) {
      return visitor(a.float_v, b.bool_v);
    } else {
      assert(false);
    }
  } else if(a.tag == TermContainer::T_bool) {
    if(b.tag == TermContainer::T_bool) {
      return visitor(a.bool_v, b.bool_v);
    } else if(b.tag == TermContainer::T_int) {
      return visitor(a.bool_v, b.int_v);
    } else if(b.tag == TermContainer::T_float) {
      return visitor(a.bool_v, b.float_v);
    } else {
      assert(false);
    }
  } else {
    assert(false);
  }
}

TermContainer operator+(const TermContainer &a, const TermContainer &b) {
  return visitBinaryOp([](auto av, auto bv) { return TermContainer(av + bv); }, a, b);
}

TermContainer operator-(const TermContainer &a, const TermContainer &b) {
  return visitBinaryOp([](auto av, auto bv) { return TermContainer(av - bv); }, a, b);
}

TermContainer operator/(const TermContainer &a, const TermContainer &b) {
  return visitBinaryOp([](auto av, auto bv) { return TermContainer(av / bv); }, a, b);
}

TermContainer operator*(const TermContainer &a, const TermContainer &b) {
  return visitBinaryOp([](auto av, auto bv) { return TermContainer(av * bv); }, a, b);
}

  // would be nice to replace with the <=> operator but it is not avaliable yet... sigh
bool operator==(const TermContainer &a, const TermContainer &b) {
  return visitBinaryOp([](auto av, auto bv) { return av == bv; }, a, b);
}


TermContainer packQuote(std::string name, std::vector<const TermContainer*> elements) {
  // this needs to match the types, and lookup the name in some already build
  // container type table.  or there should be some way to statically store that
  // information on the element itself?  if we can represent that as a templated
  // parameter, thought that could get expensive in that there would be many
  // different objects for given template type.
  // the type information would end up lost as it packs/unpacks these objects
  //
  // that would potentially lead to different traces?  But that is only if we
  // are able to split the primitive types.  Otherwise we are going to find
  // ourselves with having to handle different
  return TermContainer();
}


TermContainer packQuoteFlattened(std::string name, std::vector<const TermContainer*> elements) {
  // the flattened version should build something where the objects are a flat
  // object (so no pointers) that would allow representing something as just a
  // struct with the tag information only on the outer level

  // if something has pointers to other terms
  return TermContainer();
}



bool unpackQuote(std::string name, const TermContainer &res, std::vector<TermContainer*> elements) {
  // for the packing and unpacking operations.
  // this needs to return true/false depending on if this was successful.
  if(!res.is_ptr()) return false;
  const Term *term = res.ptr;
  const TermInfo *info = term->info;
  if(info->name != name || info->arity != elements.size()) {
    // then the number of objects inside of this does not match
    return false;
  }
  for(int i = 0; i < info->arity; i++) {
    // now we are going to unpack each of the objects into its own term class
    // so we want to represent this
    term->unpack(i, *elements[i]);
  }
  return true;
}


std::ostream& operator<<(std::ostream& os, const TermContainer &t) {
  visitOp([&](auto a) { os << a; }, t);
  return os;
}


} // namespace dyna

using namespace dyna;
using namespace std;

int main(int argc, char **argv) {
  TermContainer a(123);
  TermContainer b(4356);

  TermContainer c = a * b;

  cout << c << endl;
}



class BaseTermValue {
public:
  virtual bool isDynaTerm() { return false; } // if this is like &foo(1,2,3), then that is a dyna term.  We have special handling for this case
  virtual ~BaseTermValue()=default;

#define make_method(name, arity)                \
  virtual bool name (

};

class DynaTerm : public BaseTermValue {
public:
  bool isDynaTerm() override { return true; }
  std::string name;
  int arity;
  virtual ~DynaTerm()=default;


  static DynaTerm *create() {

  }
private:
  uint8_t values[0]; // during allocation, we are going to do this sizeof(DynaTerm)+space_needed, so this will be a pointer to the end of the object
};





class DynamicFrame {

  vector<TermContainer> runtimeTerms;
  vector<bool> isBound;
  vector<uint64_t> unionBranches;


};
