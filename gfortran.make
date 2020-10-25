FC=gfortran-mp-10
LD=$(FC)

OPT=
OPT=-O2
OPT=-Ofast
DEBUG=-g -fbacktrace -fcheck=all -ffpe-trap=invalid,zero,overflow -Wall
DEBUG=
FFLAGS=-fconvert=big-endian $(OPT) $(DEBUG)

LDFLAGS=-g
LDFLAGS=

LIBS=
