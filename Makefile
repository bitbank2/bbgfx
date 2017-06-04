CFLAGS=-c -Wall -O2

all: libbbgfx.a

libbbgfx.a: bbgfx_helper.o bbgfx_asm.o
	ar -rc libbbgfx.a bbgfx_helper.o bbgfx_asm.o ;\
	sudo cp libbbgfx.a /usr/local/lib ;\
	sudo cp bbgfx.h /usr/local/include

#bbgfx.o: bbgfx.c
#	$(CC) $(CFLAGS) bbgfx.c

bbgfx_helper.o: bbgfx_helper.c
	$(CC) $(CFLAGS) bbgfx_helper.c

bbgfx_asm.o: bbgfx_asm.s
	$(CC) $(CFLAGS) bbgfx_asm.s

clean:
	rm -rf *.o libbbgfx.a

