CXXFLAGS = -std=c++11 -Wall
LDLIBS = -ldl -lffi -lstdc++

CXX = clang++
FORMAT = clang-format-3.5

ffi-glue : ffi-glue.o

ffi-glue.o : ffi-glue.cc

.PHONY : format clean test

format :
	$(FORMAT) -style=google -i *.cc

clean :
	$(RM) *.o ffi-glue

test : ffi-glue
	 ## rand()
	echo -n 'k0w0Cp0w4MrandSco' | ./$<
	 ## cos(1.2)
	echo -n 'd1.2d0d0w1Cp0w3McosSco' | ./$<
