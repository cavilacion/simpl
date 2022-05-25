sample: main.c simpson.c racket-main.c sample.c distributions.c
	gcc -o $@ -lm main.c simpson.c racket-main.c sample.c distributions.c

simpson: simpson.c
	gcc -o simpson -lm simpson.c

clean:
	rm *.o
