CFLAGS = -W -Wall -Wextra -O2
LDLIBS = -ldl -lffi

ffi-helper : ffi-helper.o stack.o numpool.o

stack.o : stack.c stack.h
numpool.o : numpool.c numpool.h
ffi-helper.o : ffi-helper.c stack.h numpool.h

.PHONY : clean indent

indent :
	indent *.c

clean :
	$(RM) *.o ffi-helper
