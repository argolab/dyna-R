#include "terms.hpp"

#include <pybind11/pybind11.h>
namespace py = pybind11;

#include "exceptions.hpp"

using namespace dyna;



template<typename T> struct PyTermContainerTemplate {};

template<>
struct PyTermContainerTemplate<dyna::Term> {
  dyna::TermContainer container;
  //PyTermContainer(dyna::TermContainer &c) : container(c) {}
  PyTermContainerTemplate(const dyna::Term *ptr) { container.set_pointer(ptr); }
};

namespace pybind11 { namespace detail {
    template<>
    struct holder_helper<PyTermContainerTemplate<Term>> { // <-- specialization
        static const Term *get(const PyTermContainerTemplate<Term> &p) {
          return p.container.is_ptr() ? p.container.get_ptr() : nullptr; }
    };
}}


PYBIND11_DECLARE_HOLDER_TYPE(T, PyTermContainerTemplate<T>, true);


using PyTermContainer = PyTermContainerTemplate<Term>;


namespace dyna {

const TermInfo StaticIntTag("primitive_int", 4);
const TermInfo StaticFloatTag("primitive_float", 4);
const TermInfo StaticBoolTag("primitive_bool", 1);

const TermInfo StaticInt64Tag("primitive_int64", 8);
const TermInfo StaticFloat64Tag("primitive_float64", 8);


// class PythonTermWrapper {
// private:
//   Term *term;

// public:
//   ~PythonTermWrapper() {

//   }
// };

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

py::object cast_to_python(const Term *term, uint32_t idx) {
  const TermInfo *info = term->info;
  const TermInfo *nested_info = info->args[idx].term;
  if(nested_info == &StaticIntTag) {
    return py::cast(*(int32_t*)get_address(term, idx));
  } else if(nested_info == &StaticFloatTag) {
    return py::cast(*(float*)get_address(term, idx));
  } else if(nested_info == &StaticBoolTag) {
    return py::cast(*(bool*)get_address(term, idx));
  } else if(nested_info == &StaticInt64Tag) {
    return py::cast(*(int64_t*)get_address(term, idx));
  } else if(nested_info == &StaticFloat64Tag) {
    return py::cast(*(double*)get_address(term, idx));
  } else {
    if(info->args[idx].embedded) {
      // this is some embedded value, which means that it is going to have to copy the value
      throw DynaException("not embedded copying the term from the existing values");
      // this could also have some way of keeping the original object but just with some pointer
      // to the root object? but then that is going to have to reimplement all of the methods
      // so it would be easier to just copy in this case
    } else {
      // this is something that it will
      const Term *t = (const Term*)get_address(term, idx);
      PyTermContainer tc(t);
      return py::cast(tc);
    }
  }
}


PyTermContainer construct_term(const std::string, py::tuple &args) {
  Term *result = new Term;
  // this needs to lookup what kinds of types are referenced from this object
  return PyTermContainer(result);
}

void define_term_module(py::module &m) {

  m.def("term_constructor", &construct_term);

  py::class_<Term>(m, "Term")
    .def_property_readonly("name", [](const Term &t) -> const std::string& {
      if(t.info) {
        return t.info->name;
      } else {
         throw DynaException("term is null");
      }
    })
    .def_property_readonly("arity", [](const Term &t) -> uint32_t {
      if(t.info) {
        return t.info->arity;
      } else {
        throw DynaException("term is null");
      }
    })
    .def("get_argument", [](const Term &t, uint32_t idx) -> py::object {
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
    // .def(py::init([](const std::string& name, py::tuple &args) {
    //   // this is going to
    // });

}

}
