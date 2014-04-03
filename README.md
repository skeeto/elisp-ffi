# Emacs Lisp Foreign Function Interface

This library provides an FFI for Emacs Lisp so that Emacs programs can
invoke functions in native libraries. It works by driving a subprocess
to do the heavy lifting, passing result values on to Emacs.

## Examples

A function signature is described by a vector of type keywords. The
first element is the return value and the rest are the types of the
arguments. Integers, floats, strings, and pointer values obtained from
previous calls are allowed as arguments.

~~~el
;; srand(0) and rand()
(ffi-call nil "srand" [:void :uint32] 0)
;; => :void
(ffi-call nil "rand" [:sint32])
;; => 1102520059

;; cos(1.2)
(ffi-call "libm.so" "cos" [:double :double] 1.2)
;; => 0.362357754476674

;; call getenv
(ffi-get-string (ffi-call nil "getenv" [:pointer :pointer] "DISPLAY"))
;; => ":0"
~~~

The function signatures (CIFs) and handles are cached so that they
don't need to be redefined/fetched/loaded by the subprocess on each
function invocation.

Getting the function signature wrong may crash the subprocess (the FFI
"context"), but Emacs itself will be safe from crashes.

## Support

You'll need a C++11 compiler and [libffi][libffi] in order to build
the native helper program. Run `make` before attempting to use the FFI
from within Emacs.

Like the underlying libffi, there is no explicit support for calling
variadic functions, but it may still sometimes work.

There's no support for parsing/handling structs, so these must be read
raw. This will probably not change.

There's not *yet* any support for building closures, so that native
functions can call back to Emacs.

Emacs will not be able to handle very large integer values, because
Emacs' primitive integers are always slightly smaller than the
system's word size (tagged ints). This is especially an issue with
32-bit Emacs. Pointers are stored as symbols, evading these integer
limitation.

## Internals

The helper subprocess is a stack machine that operates by a very
simple, human-readable bytecode. The bytecode itself is primarily
documented in the C++ source. Emacs pushes values onto the stack and
invokes native function calls on them, leaving the result on the
stack.


[libffi]: http://sourceware.org/libffi/
