CFLAGS=-c -Wall -marm -O2

all: libbbgfx.a

libbbgfx.a: bbgfx_helper.o bbgfx_asm.o mini_io.o mini_pil.o
	ar -rc libbbgfx.a bbgfx_helper.o mini_io.o mini_pil.o bbgfx_asm.o ;\
	sudo cp libbbgfx.a /usr/local/lib ;\
	sudo cp bbgfx.h /usr/local/include

#bbgfx.o: bbgfx.c
#	$(CC) $(CFLAGS) bbgfx.c

bbgfx_helper.o: bbgfx_helper.c
	$(CC) $(CFLAGS) bbgfx_helper.c

bbgfx_asm.o: bbgfx_asm.s
	$(CC) $(CFLAGS) bbgfx_asm.s

mini_io.o: mini_io.c
	$(CC) $(CFLAGS) mini_io.c

mini_pil.o: mini_pil.c
	$(CC) $(CFLAGS) mini_pil.c

clean:
	rm *.o libbbgfx.a

