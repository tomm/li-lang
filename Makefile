%.o: %.c
	gcc -Wall -O0 -g -c $<

default: lic

objs=main.o parser.o tokenizer.o vec.o

lic: $(objs)
	gcc $(objs) -g -o lic

clean:
	-rm *.o lic
