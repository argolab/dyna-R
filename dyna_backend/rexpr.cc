namespace dyna {

class RewriteBaseOperation {
public:
  virtual char* functor_name() const=0;
  virtual uint functor_arity() const=0;

  virtual TermContainer doRewrite(const TermContainer &term)=0;
};


class RewriteAddOperation {
public:
  virtual char* functor_name() const { return "builtin_add"; }
  virtual int functor_arity() const { return 3; }

  virtual TermContainer doRewrite(const TermContainer &term)=0;
}


void define_rexpr_module(py::module &m) {

}
}


// if the variables are renammed, then that is just the term info which needs to get changed.  The values themselves would just be some offsets from the based pointer.  I suppose that the variables could also be offset in such a way that it would find that there are places


// so it would have VariableUnbound(name)
// primitiveType(offset).  The rewrite would be then VariableUnbound(name) -> primitiveType(offset)
// those rewrites would be done in such a way that it might find that there are values


// this does not quite work as the values which


// if the frame is write only with some append method, and then it finds itself doing successive rewrites of the expression, then it would be moving to some point where there are expressions that correspond with

// the term value could be something that is special with which of the expressions might be encoutered.  If there is something that it would
