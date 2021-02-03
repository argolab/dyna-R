/**
 * there should be data structures which are
 */

#include "terms.hpp"


namespace dyna {

typedef std::unordered_map<TermContainer, TermContainer> hashmap_T;
const TermInfo GenericMapTag {
  .n_bytes = sizeof(hashmap_T),
  .name = "generic_map",
  .held_type = &typeid(hashmap_T),
  .store_from_python = [](const TermInfo *info, void *address, py::handle *obj) -> void {
  },
  .to_string = [](const TermInfo *info, const void *address) -> std::string {
    return "string of hash map";
  },
  .custom_deallocator = [](const TermInfo *info, void *address) -> void {
    (*(hashmap_T)address)->~hashmap_T();
  },
  .custom_copy = [](const TermInfo *info, const void *source, void *dest) {
    new (dest) (*(hashmap_T)(dest));
  }
};

// the has map should represent which operations are there for some of the values

}
