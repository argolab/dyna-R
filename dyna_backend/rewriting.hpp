#ifndef _DYNA_REWRITING
#define _DYNA_REWRITING

namespace dyna {

struct RewriteContext {
  vector<Frame*> frames;  // the frame reference for a given value
};

struct RewritingVisitor {
  Term *rewrite(const TermInfo *info, const void *address);  // because these are nested in the structure, this is going to have to identif which of the structures are pewawnr
};

  // if there is something that is nested, then it would be best if this was able to identify that it has the same pattern multiple times.

  // if there is some pattern matching information about which data would be present.

}

#endif
