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

typedef dyna::TermContainer DynaPyTermType;
template<>
struct PyTermContainerTemplate<DynaPyTermType> {
  dyna::TermContainer container;
  PyTermContainerTemplate(dyna::TermContainer &c) : container(c) {}
  PyTermContainerTemplate(const dyna::Term *ptr) { container.set_pointer(ptr); }
  PyTermContainerTemplate(const PyTermContainerTemplate &o) : container(o.container) {}
  PyTermContainerTemplate() {}
};

namespace pybind11 { namespace detail {
    template<>
    struct holder_helper<PyTermContainerTemplate<DynaPyTermType>> { // <-- specialization
        static const DynaPyTermType *get(const PyTermContainerTemplate<DynaPyTermType> &p) {
          return &p.container;
          //return p.container.is_ptr() ? p.container.get_ptr() : nullptr;
        }
    };
}}


PYBIND11_DECLARE_HOLDER_TYPE(T, PyTermContainerTemplate<T>, true);


using PyTermContainer = PyTermContainerTemplate<DynaPyTermType>;


namespace dyna {

const TermInfo StaticIntTag {
  .n_bytes = 4,
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
  }
};
const TermInfo StaticFloatTag {
  .n_bytes = 4,
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
  }
};
const TermInfo StaticBoolTag {
  .n_bytes = 1,
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
  }
};

const TermInfo StaticInt64Tag {
  .n_bytes = 8,
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
  }
};

const TermInfo StaticFloat64Tag {
  .n_bytes = 8,
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


Term::operator std::string() const {
  return info->to_string(info, this);
}

Term *TermInfo::allocate() const {
  Term *ret = (Term*)malloc(sizeof(Term)+n_bytes);
  ret->info = this;
  return ret;
}

void TermInfo::deallocate(Term *term) const {
  // this needs to go through all of the nested values and deallocate those as well
  // in the case that those exist
  // so this should track if there is some value which requires alternate deallocation methods
  if(term->info->custom_deallocator != nullptr) {
    assert(false); // need to define what this interface is doing???
    term->info->custom_deallocator(term);
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

  // if(nested_info == &StaticIntTag) {
  //   return py::cast(*(int32_t*)get_address(term, idx));
  // } else if(nested_info == &StaticFloatTag) {
  //   return py::cast(*(float*)get_address(term, idx));
  // } else if(nested_info == &StaticBoolTag) {
  //   return py::cast(*(bool*)get_address(term, idx));
  // } else if(nested_info == &StaticInt64Tag) {
  //   return py::cast(*(int64_t*)get_address(term, idx));
  // } else if(nested_info == &StaticFloat64Tag) {
  //   return py::cast(*(double*)get_address(term, idx));
  // } else {
  //   if(info->args[idx].embedded) {
  //     // this is some embedded value, which means that it is going to have to copy the value
  //     throw DynaException("not embedded copying the term from the existing values");
  //     // this could also have some way of keeping the original object but just with some pointer
  //     // to the root object? but then that is going to have to reimplement all of the methods
  //     // so it would be easier to just copy in this case
  //   } else {
  //     // this is something that it will
  //     const Term *t = (const Term*)get_address(term, idx);
  //     PyTermContainer tc(t);
  //     return py::cast(tc);
  //   }
  // }
}

const TermInfo *construct_term_info(const py::handle &h, size_t &size) {
  if(py::isinstance<py::bool_>(h)) {
    size += 1;
    return &StaticBoolTag;
  } else if(py::isinstance<py::int_>(h)) {
    // check what the size of this value is
    size += 4;
    return &StaticIntTag;
  } else if(py::isinstance<py::float_>(h)) {
    size += 4;
    return &StaticInt64Tag;
  } else if(py::isinstance<PyTermContainer>(h)) {
    assert(false); // this needs to identify which term is nested here, then it handle something
  }
  return nullptr;
}

PyTermContainer construct_term(const std::string name, py::tuple &args) {
  TermInfo *info = new TermInfo;
  info->arity = py::len(args);
  info->unique_info = true;
  info->name = name;
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

PyTermContainer construct_any_term(py::object &value) {
  TermContainer ret;
  if(py::isinstance<py::bool_>(value)) {
    ret = TermContainer(py::cast<bool_d>(value));
  } else if(py::isinstance<py::int_>(value)) {
    ret = TermContainer(py::cast<int_d>(value));
  } else if(py::isinstance<py::float_>(value)) {
    ret = TermContainer(py::cast<float_d>(value));
  } else if(py::isinstance<py::str>(value) ||
            py::isinstance<py::dict>(value) ||
            py::isinstance<py::set>(value) ||
            py::isinstance<py::tuple>(value) ||
            py::isinstance<py::list>(value)) {
    throw DynaException("not implemented casting higher order structured types");
  } else {
    throw DynaException("type unknown");
  }
  PyTermContainer ptc(ret);
  return ptc;
}

// std::string get_string_rep(const TermContainer &tc) {
//   if(tc.is_ptr()) {
//     // then this is not a primitive type, so this can just get pushed through
//     // otherwise this is
//   }
// }

void define_term_module(py::module &m) {

  m.def("term_constructor", &construct_term);

  py::class_<TermContainer>(m, "Term")

    .def("__str__", [](const PyTermContainer &t) -> const std::string {
      if(t.container.ptr == nullptr) {
        return "None";
      } else if(t.container.is_ptr()) {
        // this is a pointer, this is going to have to print the term value
        return (std::string)(*t.container.ptr);
      } else {
        switch(t.container.tag) {
        case TermContainer::T_int: return std::to_string(t.container.int_v);
        case TermContainer::T_float: return std::to_string(t.container.float_v);
        case TermContainer::T_bool: return t.container.bool_v ? "True" : "False";
        case TermContainer::T_nonground: return "non-ground";
        default: { __builtin_unreachable(); return "None"; }
        }
      }
    })
    .def_property_readonly("name", [](const PyTermContainer &t) -> const std::string& {
      if(t.container.is_ptr()) {
        return t.container.ptr->info->name;
      } else {
        throw DynaException("term is null");
      }
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
