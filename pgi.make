FC=pgfortran
LD=$(FC)

OPT=
OPT=-O2
OPT=-fast
DEBUG=-g -traceback -Mbounds -Ktrap=fp
DEBUG=
FFLAGS=-byteswapio $(OPT) $(DEBUG)

LDFLAGS=-g
LDFLAGS=

LIBS=
