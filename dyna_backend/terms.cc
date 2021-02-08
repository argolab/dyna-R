#include "terms.hpp"
#include "exceptions.hpp"

#include <stdlib.h>

#include <pybind11/pybind11.h>
namespace py = pybind11;

#include <sstream>

using namespace dyna;

using namespace std;


// this is only defined for 1 type, as this allows us to do special things with this type
// otherwise it would have that this is is something else
template<typename T> struct PyTermContainerTemplate {};

typedef dyna::Term DynaPyTermType;
template<>
struct PyTermContainerTemplate<DynaPyTermType> {
private:
  dyna::Term *ptr;
public:
  ~PyTermContainerTemplate() {
    if(ptr != nullptr) ptr->decr_ref();
    ptr = nullptr;
  }
  PyTermContainerTemplate(const PyTermContainerTemplate &oth) : ptr(oth.ptr) { if(ptr != nullptr) ptr->incr_ref(); }
  PyTermContainerTemplate(dyna::Term *ptr) : ptr(ptr) { if(ptr != nullptr) ptr->incr_ref(); }
  PyTermContainerTemplate() : ptr(nullptr) {}

  PyTermContainerTemplate &operator=(const PyTermContainerTemplate &other) {
    dyna::Term *old = ptr;
    ptr = other.ptr;
    ptr->incr_ref();
    if(old != nullptr) old->decr_ref();
    return *this;
  }

  dyna::Term* get_ptr() const { return ptr; }

  // dyna::TermContainer container;
  // PyTermContainerTemplate(dyna::TermContainer &c) : container(c) {}
  // PyTermContainerTemplate(const dyna::Term *ptr) { container.set_pointer(ptr); }
  // PyTermContainerTemplate(const PyTermContainerTemplate &o) : container(o.container) {}
  // PyTermContainerTemplate() {}

  dyna::Term* operator->() const { return ptr; }
  operator bool() const { return ptr != nullptr; }
};

namespace pybind11 { namespace detail {
    template<>
    struct holder_helper<PyTermContainerTemplate<DynaPyTermType>> { // <-- specialization
        static const DynaPyTermType *get(const PyTermContainerTemplate<DynaPyTermType> &p) {
          return p.get_ptr();
          //return &p.container;
          //return p.container.is_ptr() ? p.container.get_ptr() : nullptr;
        }
    };
}}


PYBIND11_DECLARE_HOLDER_TYPE(T, PyTermContainerTemplate<T>); //, true);


using PyTermContainer = PyTermContainerTemplate<DynaPyTermType>;


namespace dyna {

const TermInfo StaticIntTag {
  .n_bytes = sizeof(int32_t),
  .name = "primitive_int",
  .held_type = &typeid(int32_t),
  .store_from_python = [](const TermInfo *info, void *address, py::handle *obj) -> void {
    *((int32_t*)address) = obj->cast<int32_t>();
  },
  .cast_to_python = [](const TermInfo *info, const void *address) -> py::object {
    return py::cast(*(int32_t*)address);
  },
  .to_string = [](const TermInfo *info, const void *address) -> std::string {
    return to_string(*(int32_t*)address);
  },
  .walk_all_pointers = [](const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent_metadata), void *transparent) {}
};
const TermInfo StaticFloatTag {
  .n_bytes = sizeof(float),
  .name = "primitive_float",
  .held_type = &typeid(float),
  .store_from_python = [](const TermInfo *info, void *address, py::handle *obj) -> void {
    *((float*)address) = obj->cast<float>();
  },
  .cast_to_python = [](const TermInfo *info, const void *address) -> py::object {
    return py::cast(*(float*)address);
  },
  .to_string = [](const TermInfo *info, const void *address) -> std::string {
    return to_string(*(float*)address);
  },
  .walk_all_pointers = [](const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent), void *transparent) {}
};
const TermInfo StaticBoolTag {
  .n_bytes = sizeof(bool),
  .name = "primitive_bool",
  .held_type = &typeid(bool),
  .store_from_python = [](const TermInfo *info, void *address, py::handle *obj) -> void {
    *((bool*)address) = obj->cast<bool>();
  },
  .cast_to_python = [](const TermInfo *info, const void *address) -> py::object {
    return py::cast(*(bool*)address);
  },
  .to_string = [](const TermInfo *info, const void *address) -> std::string {
    return to_string(*(bool*)address);
  },
  .walk_all_pointers = [](const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent), void *transparent) {}
};

const TermInfo StaticInt64Tag {
  .n_bytes = sizeof(int64_t),
  .name = "primitive_int64",
  .held_type = &typeid(int64_t),
  .store_from_python = [](const TermInfo *info, void *address, py::handle *obj) -> void {
    *((int64_t*)address) = obj->cast<int64_t>();
  },
  .cast_to_python = [](const TermInfo *info, const void *address) -> py::object {
    return py::cast(*(int64_t*)address);
  },
  .to_string = [](const TermInfo *info, const void *address) -> std::string {
    return to_string(*(int64_t*)address);
  },
  .walk_all_pointers = [](const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent), void *transparent) {}
};

const TermInfo StaticFloat64Tag {
  .n_bytes = sizeof(double),
  .name = "primitive_float64",
  .held_type = &typeid(double),
  .store_from_python = [](const TermInfo *info, void *address, py::handle *obj) -> void {
    *((double*)address) = obj->cast<int64_t>();
  },
  .cast_to_python = [](const TermInfo *info, const void *address) -> py::object {
    return py::cast(*(double*)address);
  },
  .to_string = [](const TermInfo *info, const void *address) -> std::string {
    return to_string(*(double*)address);
  },
  .walk_all_pointers = [](const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent), void *transparent) {}
};

const TermInfo StaticGenericTermPointer {
  .n_bytes = sizeof(Term*),
  .name = "generic_pointer",
  .held_type = &typeid(Term*),
  .store_from_python = [](const TermInfo *info, void *address, py::handle *obj) -> void {
    // if the address is already set with something, then this would need to clear it?
    // though this is going to be immutable objects, so we are not going to be updating it once something is stored
    Term *t = obj->cast<Term*>();
    t->incr_ref();
    *((Term**)address) = t;
  },
  .cast_to_python = [](const TermInfo *info, const void *address) -> py::object {
    Term *t = *((Term**)address);
    PyTermContainer ptc(t);
    return py::cast(ptc);
  },
  .to_string = [](const TermInfo *info, const void *address) -> std::string {
    Term *t = *((Term**)address);
    return (std::string)(*t);
  },
  .custom_deallocator = [](const TermInfo *info, void *address) -> void {
    Term *t = *((Term**)address);
    t->decr_ref();
  },
  .walk_all_pointers = [](const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent), void *transparent) {
    *static_cast<void**>(self) = walker_function(*static_cast<void**>(self), transparent);
  }
};

const void *get_address(const Term *term, uint32_t idx) {
  const TermInfo *info = term->info;
  const NestedTermInfo *ni = &info->args[idx];
  if(ni->embedded) {
    return term->values + ni->offset;
  } else {
    const uint8_t *ptr = term->values + ni->offset;
    return *(void**)ptr;
  }
}
// const overloaded...
void *get_address(Term *term, uint32_t idx) {
  const TermInfo *info = term->info;
  const NestedTermInfo *ni = &info->args[idx];
  if(ni->embedded) {
    return term->values + ni->offset;
  } else {
    const uint8_t *ptr = term->values + ni->offset;
    return *(void**)ptr;
  }
}


std::string TermInfo_defaultToString(const TermInfo *info, const void *address) {
  const Term *term = (const Term*)address;
  stringstream out;
  out << "&";
  out << info->name;
  if(info->arity > 0) {
    out << "(";
    for(uint i = 0; i < info->arity; i++) {
      if(i != 0) out << ", ";
      const NestedTermInfo *ni = &info->args[i];
      out << ni->term->to_string(ni->term, get_address(term, i));
    }
    out << ")";
  }
  return out.str();
}

void TermInfo_defaultStructuredTerm_storedFromPython(const TermInfo *info, void *address, py::handle *obj) {
  // this destination values should have already be constructed such that it is large enough
  Term *self = obj->cast<Term*>();
  // for(uint i = 0; i < info->arity; i++) {
  //   NestedTermInfo *ni = &info->args[i];
  //   ni->term->store_from_python(ni->term, ((uint8_t*)address) + ni->offset, self
  // }
  // this can just memcopy the values.  If there is something that requires advanced copy
  memcpy(address, self->values, self->info->n_bytes);
  // if there is something that is non-trivial, then we need to handle that.
  // the non-trivial would be something like it has a pointer
}

py::object TermInfo_defaultStructuredTerm_castToPython(const TermInfo *info, const void *address) {
  return py::none();
}

void TermInfo_defaultStructuredTerm_walkAllPointers(const TermInfo *info, void *self, void *(*walker_function)(void *ptr, void *transparent), void *transparent) {
  Term *term = (Term*)self;
  for(uint i = 0; i < info->arity; i++) {
    const NestedTermInfo *ni = &info->args[i];
    ni->term->walk_all_pointers(ni->term, get_address(term, i), walker_function, transparent);
  }
}

Term::operator std::string() const {
  return info->to_string(info, this);
}

bool Term::operator==(const Term &other) const {
  if(this == &other) return true;
  if(info != other.info) return false;
  if(hash() != other.hash()) return false;

  // TODO: more stuff using equality methods?  I suppose those are going to have to get indirected through what kinds of nested data types there are as well...

  assert(false);
}

Term *TermInfo::allocate() const {
  Term *ret = (Term*)malloc(sizeof(Term)+n_bytes);
  ret->info = this;
  ret->ref_count = 0;
  ret->hashcache = 0;
  return ret;
}

void TermInfo::deallocate(Term *term) const {
  // this needs to go through all of the nested values and deallocate those as well
  // in the case that those exist
  // so this should track if there is some value which requires alternate deallocation methods
  if(term->info->custom_deallocator != nullptr) {
    assert(false); // need to define what this interface is doing???
    term->info->custom_deallocator(term->info, term->values);
  }
  if(term->info->unique_info) {
    // then this should deallocate which of the values
    free(const_cast<TermInfo*>(term->info));
  }

  free(term);
}

py::object cast_to_python(const Term *term, uint32_t idx) {
  const TermInfo *info = term->info;
  const TermInfo *nested_info = info->args[idx].term;
  if(nested_info->cast_to_python != nullptr) {
    return nested_info->cast_to_python(nested_info, get_address(term, idx));
  } else {
    throw DynaException("undefined casting to python method");
  }
}

const TermInfo *construct_term_info(const py::handle &h, size_t &size) {
  if(py::isinstance<py::bool_>(h)) {
    size += 1;
    return &StaticBoolTag;
  } else if(py::isinstance<py::int_>(h)) {
    // check what the size of this value is
    size += StaticIntTag.n_bytes;
    return &StaticIntTag;
  } else if(py::isinstance<py::float_>(h)) {
    size += StaticFloatTag.n_bytes;
    return &StaticFloatTag;
  } else if(py::isinstance<DynaPyTermType>(h)) {
    Term *nested_term = py::cast<DynaPyTermType*>(h);
    const_cast<TermInfo*>(nested_term->info)->unique_info = false; // nested info is no longer going to be unique
    size += nested_term->info->n_bytes;
    return nested_term->info;
  }
  assert(false);  // this needs to get handled
  return nullptr;
}

PyTermContainer construct_term(const std::string name, py::tuple &args) {
  TermInfo *info = new TermInfo;
  info->arity = py::len(args);
  info->unique_info = true;
  info->name = name;
  info->store_from_python = TermInfo_defaultStructuredTerm_storedFromPython;
  info->cast_to_python = TermInfo_defaultStructuredTerm_castToPython;
  info->walk_all_pointers = TermInfo_defaultStructuredTerm_walkAllPointers;
  // need some way in the future to identify how to merge these
  // term values.  This would be something like looking up the different type values in some
  // identifier tree or something
  size_t size = 0;
  for(uint i = 0; i < info->arity; i++) {
    // this needs to determine what type of value this is, and then push that info
    // into the term
    size_t offset = size;
    const TermInfo *ti = construct_term_info(args[i], size);
    NestedTermInfo ni {
      .term = ti,
      .offset = (uint)offset,
      .embedded = true
    };
    info->args.push_back(ni);
  }
  info->n_bytes = size;

  Term *ret = info->allocate();
  for(uint i = 0; i < info->arity; i++) {
    const TermInfo *ni = info->args[i].term;
    py::handle h = args[i];
    ni->store_from_python(ni, get_address(ret, i), &h);
  }

  // this needs to lookup what kinds of types are referenced from this object
  return PyTermContainer(ret);
}

void define_term_module(py::module &m) {

  m.def("term_constructor", &construct_term);

  py::class_<DynaPyTermType, PyTermContainer>(m, "Term")

    .def("__str__", [](const PyTermContainer t) -> const std::string {
      if(!t) {
        return "None";
      } else {
        return (std::string)(*t.get_ptr());
      }
    })
    .def_property_readonly("name", [](const PyTermContainer t) -> const std::string& {
      if(t) {
        return t->info->name;
      } else {
        throw DynaException("term is null");
      }
    })
    .def_property_readonly("arity", [](const PyTermContainer t) -> uint {
      return t->info->arity;
    })
    .def("builtin_eq", [](const PyTermContainer self, const PyTermContainer other) -> bool {
      return false;
    })
    .def("__hash__", [](const PyTermContainer self) -> hashcode_T {
      return 0;
    })
    .def_static("fromlist", [](py::object &obj) {
      return py::none();
    })
    .def("make_list", [](const PyTermContainer self) {
      // to a list in the language from its argumens '.'(1, '.'(2, nil()))
      return py::none();
    })
    .def("make_pylist", [](const PyTermContainer self) {
      // make a list which is python [1,2]
      return py::none();
    })
    .def("get_argument", [](const PyTermContainer self, uint32_t idx) {
      return cast_to_python(self.get_ptr(), idx);
    })
    .def_property_readonly("number_bytes", [](const PyTermContainer self) {
      return self->info->n_bytes + sizeof(Term);
    });
    /*
    .def_property_readonly("name", [](const TermContainer &t) -> const std::string& {
      if(t.info) {
        return t.info->name;
      } else {
         throw DynaException("term is null");
      }
    })
    .def_property_readonly("arity", [](const TermContainer &t) -> uint32_t {
      if(t.info) {
        return t.info->arity;
      } else {
        throw DynaException("term is null");
      }
    })
    .def("get_argument", [](const TermContainer &t, uint32_t idx) -> py::object {
      if(t.info) {
        // this needs to determine which of the values
        if(idx >= t.info->arity) {
          throw DynaException("term out of bounds for accessing arity");
        }
        return cast_to_python(&t, idx);
      } else {
        throw DynaException("term is null");
      }
    });

  */

    // .def(py::init([](const std::string& name, py::tuple &args) {
    //   // this is going to
    // });

}

}
