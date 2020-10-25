FC=ifort
LD=$(FC)

OPT=
OPT=-O2
OPT=-Ofast
DEBUG=-g -traceback -check all -fpe0 -init=snan,arrays -ftrapuv -fp-stack-check -debug
DEBUG=
FFLAGS=-fp-model strict -convert big_endian -assume byterecl -ftz $(OPT) $(DEBUG)

LDFLAGS=-g
LDFLAGS=

LIBS=
