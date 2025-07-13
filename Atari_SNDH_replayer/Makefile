
OUT = sndh.bin

SRC = main.s

AS = ../../vbcc/bin/vasmm68k_mot
AS_FLAGS = -quiet -m68000 -devpac -align -pic -Fbin -L main.lst

all:
	$(AS) $(AS_FLAGS) -o $(OUT) $(SRC)

clean:
	rm -f $(OUT)
