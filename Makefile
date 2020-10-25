TARGET=gfb3
SRC=types.f90 dimensions.f90 halo.f90 masks.f90 inicon.f90 diagnostics.f90 gfb3.f90

COMPILER ?= gfortran
ifeq (,$(wildcard $(COMPILER).make))
    $(error "${COMPILER}" compiler not supported!)
else
    include $(COMPILER).make
endif
$(info Using compiler "${COMPILER}")

.SUFFIXES:
.SUFFIXES: .f90 .o

OBJ=$(SRC:.f90=.o)

.f90.o:
	$(FC) $(FFLAGS) $(INC) -c $<

$(TARGET): $(OBJ)
	$(LD) -o $(TARGET) $(LDFLAGS) $(OBJ) $(LIBS)

all: $(TARGET)
clean:
	rm -f *.mod *.o $(TARGET)
