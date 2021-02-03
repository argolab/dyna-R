#ifndef _DYNA_FRAME
#define _DYNA_FRAME

namespace dyna {

  struct Frame {
    TermContainer *slots;
    uint64_t *slots_allocated;  // a bit map which indicates which slots are currently allocated, this should be a multiple of 64 slots in the system
    uint32_t n_slots;
  };

  struct FrameWrapper {

  };

  struct VariableUnbound {

  };


  struct VariableBound : TermInfo {
    //
  };

}

#endif
