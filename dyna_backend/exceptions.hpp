#ifndef _DYNA_EXCEPTIONS
#define _DYNA_EXCEPTIONS

namespace dyna {

class DynaException : std::exception {
private:
  std::string msg;
  const char *cmsg;
public:
  DynaException(std::string m) : msg(m), cmsg(msg.c_str()) {}
  DynaException(const char *c) : cmsg(c) {}
  virtual const char *what() const throw() {
    return cmsg;
  }
};

}

#endif
