#include <vector>
#include <functional>

#include <boost/smart_ptr.hpp>


#include <pybind11/pybind11.h>



namespace dyna {

  enum RTypes {
    RT_unset = 0,
    RT_intersect = 1,
    RT_partition,
    RT_unify,
    RT_aggregator,
    RT_moded_op  // there needs to be a bit more dispatch that happens between these operators
  };

  class Frame {
  private:
    py::dict var_map;  // object -> int  where slots are allocated in a vector
    vector<TermContainer> slots;
  public:


    typedef int32_t id; // where -1 will represent that this is not set
  };

  // should there be a single
  class Variable {
    typename Frame::id id;
    TermContainer constant;

    Variable(TermContainer &c) : id(-1), constant(c) {}
    Variable(id id) : id(id) {}

  };

  class RBaseType {
  private:
    mutable uint32_t ref_counter;
		friend void rexpr_ptr_add_ref(const RBaseType *);
		friend bool rexpr_ptr_release(const RBaseType *);
  public:
    const RTypes type_signature; // this is probably a 4 byte int, which may be more wasteful then worth doing, or


    RBaseType(uint_t type) : type_signature(type) {}

    virtual ~RBaseType() = default;

    // call on all children
    virtual Handle<RBaseType> rewrite(std::function<RBaseType*(RBaseType*)> mapper) = 0;

    virtual Handle<RBaseType> rename_vars(std::function<Variable(Variable)> mapper) = 0;

    Handle<RBaseType> rename_vars_unique(std::function<Variable(Variable)> mapper) {
      // this should just follow the python and rename variables to have unique slots
      // though if variables are being given integer slots in a frame, then this might require a bit more access?
    }

    virtual Handle<RBaseType> simplify(Frame *f) {
      return visit(this, [=](auto *a) { return do_simplify(a, frame); });
    }
  };


  template<typename T> using Handle = boost::intrusive_ptr<T>;
  using boost::dynamic_pointer_cast;
	using boost::static_pointer_cast;


  inline void rexpr_ptr_add_ref(const RBaseType *p) {
    p->ref_counter++;
  }

  inline bool rexpr_ptr_release(const RBaseType *p) {
    assert(0 != p->ref_counter);
    return --p->ref_counter;
  }

  template <typename T> inline void intrusive_ptr_add_ref(const T *p) {
	  rexpr_ptr_add_ref(p);
	}
	template <typename T> inline void intrusive_ptr_release(const T *p) {
		if(!rexpr_ptr_release(p)) {
			delete p;
		}
	}



  auto visit(Handle<RBaseType> r, auto visitor) {
    switch(r->type_signature) {
    case RT_intersect:
      return visitor(static_ptr_cast<RIntersect>(r));
    case RT_partition:
      return visitor(static_ptr_cast<RPartition>(r));
    case RT_unify:
      return visitor(static_ptr_cast<RUnify>(r));
    case RT_aggregator:
      return visitor(static_ptr_cast<RAggregator>(r));
    case RT_moded_op:
      return visitor(static_ptr_cast<RModedOp>(r));
    default:
      return visitor(r);
    }
  }


  template<typename R, typename ...T>
  class Matcher {

    template<typename ...Args>
    R operator()(Handle<RBaseType> r, Args & args...) {
      // then should pattern match agains the T types to perform rewrites against the different operators

    }




  };



  RBaseType *do_simplify(RIntersect *self, Frame *frame) {

  }

}
