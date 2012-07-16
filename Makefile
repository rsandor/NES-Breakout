clean:
	rm breakout.o
	rm breakout.nes

all:
	ca65 -t nes breakout.s -o breakout.o
	ld65 -t nes breakout.o -o breakout.nes
