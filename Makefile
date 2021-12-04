%.o: %.c
	gcc -Wall -O0 -g -c $<

default: lic

objs=main.o parser.o tokenizer.o vec.o output_lr35902.o types.o error.o program.o

lic: $(objs)
	gcc $(objs) -g -o lic

clean:
	-rm *.o lic

testsuite:
	./lic li-code/testsuite.li
	rgbasm out.asm -o testsuite.o
	rgblink -t -o testsuite.gb testsuite.o
	rgbfix -v -p 0 testsuite.gb
	
examples:
	./lic li-code/conway.li
	rgbasm out.asm -o conway.o
	rgblink -t -o conway.gb conway.o
	rgbfix -v -p 0 conway.gb
	
	./lic li-code/benchmark.li
	rgbasm out.asm -o benchmark.o
	rgblink -t -o benchmark.gb benchmark.o
	rgbfix -v -p 0 benchmark.gb
	
	./lic li-code/etch.li
	rgbasm out.asm -o etch.o
	rgblink -t -o etch.gb etch.o
	rgbfix -v -p 0 etch.gb
