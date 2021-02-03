
#include <string>

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "exceptions.hpp"

namespace py = pybind11;

namespace dyna {
  void define_term_module(py::module&);
}


PYBIND11_MODULE(dyna_cpp_backend, m) {

  m.def("test_method", []() -> std::string { return "hello world"; });

  py::module_ terms = m.def_submodule("terms");
  dyna::define_term_module(terms);


  py::module_ rexprs = m.def_submodule("rexprs");
  dyna::define_rexpr_module(rexprs);

  static py::exception<dyna::DynaException> dyna_exception(m, "DynaBackendException");
  py::register_exception_translator([](std::exception_ptr p) {
    try {
        if (p) std::rethrow_exception(p);
    } catch (const dyna::DynaException &e) {
        dyna_exception(e.what());
    }
});
}
