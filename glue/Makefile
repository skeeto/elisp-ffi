CXXFLAGS = -std=c++11 -Wall
LDLIBS = -ldl -lffi -lstdc++

CXX = clang++
FORMAT = clang-format-3.5

ffi-glue : ffi-glue.o

ffi-glue.o : ffi-glue.cc

.PHONY : indent clean test

indent :
	$(FORMAT) -style=google -i *.cc *.hh

clean :
	$(RM) *.o ffi-glue

test : ffi-glue
	echo -n 'k0w0CNw4MrandSco' | ./$<
