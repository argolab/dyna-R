#include "terms.hpp"

#include <pybind11/pybind11.h>
namespace py = pybind11;

using namespace dyna;



template<typename T> struct PyTermContainerTemplate {};

template<>
struct PyTermContainerTemplate<dyna::Term> {
  dyna::TermContainer container;
  //PyTermContainer(dyna::TermContainer &c) : container(c) {}
  PyTermContainerTemplate(dyna::Term *ptr) { container.set_pointer(ptr); }
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



PyTermContainer construct_term(const std::string, py::tuple &args) {
  Term *result = new Term;
  // this needs to lookup what kinds of types are referenced from this object
  return PyTermContainer(result);
}

void define_term_module(py::module &m) {

  m.def("term_constructor", &construct_term);

  py::class_<Term>(m, "Term");
    // .def(py::init([](const std::string& name, py::tuple &args) {
    //   // this is going to
    // });

}

}
