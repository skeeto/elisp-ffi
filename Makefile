.POSIX:
CXX      = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -O3 $$(pkg-config --cflags libffi)
LDLIBS   = -ldl -lstdc++ $$(pkg-config --libs libffi)
EMACS    = emacs

ffi-glue: ffi-glue.cc

ffi-tests.elc: ffi-tests.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile ffi-tests.el

ffi.elc: ffi.el
	$(EMACS) -Q -batch -f batch-byte-compile ffi.el

clean:
	rm -f *.o *.elc ffi-glue

test: ffi-glue ffi.elc ffi-tests.elc
	## rand()
	echo -n 'k0w0Cp0w4MrandSco' | ./ffi-glue
	## cos(1.2)
	echo -n 'd1.2d0d0w1Cp0w3McosSco' | ./ffi-glue
	## memset(malloc(4), 0x5a, 4), read-array<u8>
	echo -n 'x4x4k90x4x0p0w1Cp0w6MmallocScx0k0p0p0w3Cp0w6MmemsetScAu' | ./ffi-glue
	$(EMACS) -batch -Q -L . -l ffi-tests.elc -f ert-run-tests-batch
