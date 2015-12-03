fc	:= ifort
#fc	:= gfortran

fflags	:= -O3

src	:= $(wildcard *.f90)

prg	:= $(src:%.f90=%)

#########################################################################

all: $(prg)

%: %.f90
	$(fc) $(fflags) $< -o $@

clean:
	rm -f *~ $(prg) *.mod
