
CPP_SOURCE=module.cc terms.cc rexpr.cc frame.cc memory_management.cc
TARGET=dyna_cpp_backend.so
FLAGS := $(shell python -m pybind11 --include) -fpic -ggdb -O0 -std=c++2a
LDFLAGS := $(shell python-config --ldflags) -lpython3

OBJS=$(CPP_SOURCE:.cc=.o)

CXX=g++

ifneq ($(OPT),)
FLAGS := $(filter-out -O0,$(FLAGS)) -O3
endif

.PHONY: module

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(FLAGS) $(LDFLAGS) -shared -o $@ $^

%.o: %.cc
	$(CXX) $(FLAGS) -c -o $@ $<

clean:
	rm -rf $(OBJS) $(TARGET)


depend:
	rm -f Makefile.deps
	for i in $(CPP_SOURCE)  ; do \
		$(CXX) -MM -MF Makefile.deps_o $(FLAGS) $$i ; \
		cat Makefile.deps_o >> Makefile.deps ; \
		rm Makefile.deps_o ; \
	done

-include Makefile.deps
