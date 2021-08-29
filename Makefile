%.o: %.c
	gcc -Wall -O0 -g -c $<

default: lic

objs=main.o parser.o tokenizer.o vec.o output_lr35902.o types.o error.o program.o

lic: $(objs)
	gcc $(objs) -g -o lic

clean:
	-rm *.o lic

example:
	./lic example.li
	rgbasm out.asm -o example.o
	rgblink -o example.gb example.o
	rgbfix -v -p 0 example.gb
	
conway:
	./lic conway.li
	rgbasm out.asm -o conway.o
	rgblink -o conway.gb conway.o
	rgbfix -v -p 0 conway.gb
