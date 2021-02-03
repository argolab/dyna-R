#include "frame.hpp"


/**
 * The slots in the frame should refernece the term container
 *
 * The R-expr could be something with the same representation in the term
 * container.  Then the r-expr itself would just be a nested term.
 *
 * The frame would just be the values that are stored in the structure itself, it would have that there are
 * variables which are just stored in some slot. The variables would get mapped in such a way that it would find
 * that those values would be present
 *
 * There could be a sequence of rewrites which would be applicable in the case that it identifies the different operations
 */
