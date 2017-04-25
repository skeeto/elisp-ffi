.POSIX:
CXX      = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -O3 $$(pkg-config --cflags libffi)
LDLIBS   = -ldl -lstdc++ $$(pkg-config --libs libffi)

ffi-glue: ffi-glue.cc

clean:
	rm -f *.o ffi-glue

test: ffi-glue
	## rand()
	echo -n 'k0w0Cp0w4MrandSco' | ./ffi-glue
	## cos(1.2)
	echo -n 'd1.2d0d0w1Cp0w3McosSco' | ./ffi-glue
	emacs -batch -Q -L . -l ffi-tests.el -f ert-run-tests-batch
