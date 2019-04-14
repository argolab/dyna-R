#include "cpp_term_class.cc"

namespace dyna {

  // wrap a free TermContainer so that we can call it
  struct Free {
    TermContainer &wrapped;
    Free(TermContainer &w) : wrapped(w) {}
    const TermContainer &operator=(const TermContainer &o) {
      return wrapped.operator=(o);
    }
  };


  // would need to include the headers for these objects in all files that depend on these
#define DYNA_TYPE_LIST(x)                \
  x(Eigen::MatrixXf, 1)                  \
  x(Eigen::MatrixXd, 2)                  \
  x(py::object, 3)                       \
  x(std::string, 4)                      \

  // the type information could also include table types?  Then we can just pass those around as normal pointers to objects
  // that would make having the prefix trie stuff not so bad.  That could return some F structure which represents the trie structure
  // along with an object which contains the reference to the table type at every point


  // divide and subtract have to be already rewritten in terms of these methods
#define DYNA_METHOD_LIST(x)                     \
  x(add, 3)                                     \
  x(multiply, 3)                                \
  x(divide, 3)                                  \





  namespace methods {
    // methods that are accessable from dyna

    // modeing is happening via the types that are passed into these objects
    // the C++ compiler should be able to fix this up

    // free ground ground
    bool add(Free &a, const TermContainer &b, const TermContainer &c) {
      a = b + c;
      return true;
    }
    // ground ground ground
    bool add(const TermContainer &a, const TermContainer &b, const TermContainer &c) {
      return a == b + c;
    }
    // ground free ground
    bool add(const TermContainer &a, Free &b, const TermContainer &c) {
      b = a - c;
      return true;
    }
    // ground ground free
    bool add(const TermContainer &a, const TermContainer &b, Free &c) {
      c = a - b;
      return true;
    }
  }



  // check if the given mode is provided via the methods this should basically
  // just run on the free/ground operations
  template <typename Callable, typename Args...>
  struct can_call_helper {
    typedef decltype(Callable(Args...)) Callable_ret;

  }


  template<typename Callable, typename ArgsT>
  bool call(Callable &t, ArgsT& ... args) {
    // calling args with something other then TermContainer should be ill defined

    // want to recurse on the arguments matching which ones are free/ground such that it can identify
  }


  namespace methods {
    bool add(TermContainer &a, const Eigen::MatrixXf &b, const Eigen::MatrixXf &c) {
      // in this case, the term container is getting the wrapped up eigen matrix class, where we are unpacking this object
      // either via some runtime information.
      //
      // so this would be nice if this could just contain the information about
      // what type this is, along with any information about what /type/.  If we could just use the virtual method inhertiance on methods,
      // then this might not be /sooo/ bad??  In which case, we could just pattern matchin against some method.
      // might even be able to do faster switch statement based dispatch.  The real goal would be to be able ot
      a = b + c;
    }
  }

}
