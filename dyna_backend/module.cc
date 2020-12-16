
#include <string>

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

namespace py = pybind11;

namespace dyna {
  void define_term_module(py::module&);
}


PYBIND11_MODULE(dyna_cpp_backend, m) {

  m.def("test_method", []() -> std::string { return "hello world"; });

  py::module_ terms = m.def_submodule("terms");
  dyna::define_term_module(terms);

}
