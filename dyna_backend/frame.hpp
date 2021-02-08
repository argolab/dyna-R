#include "terms.hpp"

// #include "robhin_hood.hpp"

namespace dyna {

  class FrameLayout;
  class Frame;
  class Variable;
  class VariableId;
  class VariableConstant;

  class FrameLayout {
  private:
    //robhin_hood::unordered_map<std::string, uint> variable_mapping;
  };

  class Frame {
  private:
    FrameLayout *layout;
    std::vector<TermContainer> slots; // as values are used, then is going to want to GC slots so that this is not wasting too much space, but that would also mean that that this is going to need rewrite the FrameLayout
    friend class Variable;
    friend class VariableId;
  };

  // class Variable {
  // public:
  //   virtual TermContainer getValue(Frame*)=0;
  //   virtual bool setValue(Frame *, TermContainer &value)=0;
  // };

  // class VariableId : public Vraiable {
  // private:
  //   std::string variable_identifier;
  //   FrameLayout *cache_layout = nullptr;
  //   uint cache_slot;
  // public:
  //   override TermContainer getValue(Frame *frame) {
  //     if(cache_layout != frame->layout) {
  //       cache_slot = frame->layout->variable_mapping[variable_identifier];
  //       cache_layout = frame->layout;
  //     }
  //     return frame->slots[cache_slot];
  //   }
  //   override bool setValue(Frame *frame, TermContainer &value) {
  //     if(cache_layout != frame->layout) {
  //       cache_slot = frame->layout->variable_mapping[variable_identifier];
  //       cache_layout = frame->layout;
  //     }
  //     return frame->slots[cache_slot].unify_ground(value);
  //   }
  // };

  // class VariableConstant : public Variable {
  // private:
  //   TermContainer value;
  // public:
  //   override TermContainer getValue(Frame*) { return value; }
  //   override bool setValue(Frame*, TermContainer &value) {
  //     return value == this->value;
  //   }
  //   VariableConstant(TermContainer v) : value(v) {}
  // };

}
