# ====================================================================== #
#                                                                        #
#            Makefile for LagPat                                         #
#                   2007.05.29 by NISHIMURA, Nobuya                      #
#                                                                        #
#            update log:                                                 #
#              2008.07.01                                                #
#                - modified for module build (N.N.)                      #
# ====================================================================== #

FC	:= ifort
#FC	:= gfortran

FCFLAGS	:= -O3
#FCFLAGS	:= -check all -warn all -traceback

PRG	:= lagpat

MAIN	:= $(PRG:%=%.f90)

SRCMOD	:= mod_cnst.f90 mod_set.f90 mod_fld.f90

MOD	:= $(SRCMOD:%.f90=%.mod)

OBJMOD	:= $(SRCMOD:%.f90=%.o)

SRC	:= $(filter-out $(SRCMAIN) $(SRCMOD), $(wildcard *.f90))

OBJ	:= $(SRC:%.f90=%.o)

LIBPRG	:= lib$(PRG).a

LIBOBJ	:= $(OBJ:%.o=$(LIBPRG)(%.o))

LIBMOD	:= $(OBJMOD:%.o=$(LIBPRG)(%.o))

#=============================================================================

all:	$(PRG)

$(PRG):	%: %.f90 $(LIBPRG) $(LIBOBJ) $(LIBMOD)
	$(FC) $(FCFLAGS)  -o $@ $< -L. $(LIBPRG)

$(LIBPRG): $(LIBOBJ)

$(LIBOBJ): $(LIBMOD)

%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

clean:
	rm -f *.o *__* *~ $(LIBPRG) $(MOD) $(NTWK) $(PRG) fort.*
