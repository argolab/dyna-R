
CPP_SOURCE=module.cc terms.cc
TARGET=dyna_cpp_backend.so
FLAGS := $(shell python -m pybind11 --include) -fpic -ggdb -O0
LDFLAGS := $(shell python-config --ldflags) -lpython3

OBJS=$(CPP_SOURCE:.cc=.o)

CXX=g++

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
	for i in $(addprefix dyna_backend/,$(CPP_SOURCE))  ; do \
		$(CXX) -MM -MF Makefile.deps_o $(FLAGS) $$i ; \
		cat Makefile.deps_o >> Makefile.deps ; \
		rm Makefile.deps_o ; \
	done

-include Makefile.deps
