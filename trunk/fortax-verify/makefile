
MODPATH = ../fortax
FORTAXLIB = ../fortax/fortax.a

# ------------------Macro-Defs---------------------
FFLAGS = -O0 -g -traceback -fpp -check bounds -check all -warn unused -stand f03 -fPIC -gen-interfaces -module $(MODPATH)
DIAGDISABLE = -diag-disable 5268,7025
# FFLAGS = -O3 -fpp -stand f03 -warn all -inline speed -inline-forceinline -no-prec-div -xHost -static -fPIC -gen-interfaces $(DIAGDISABLE) $(GPROF) -module $(MODPATH)
F90 = ifort
# -------------------End-macro-Defs---------------------------

all:fortax_verify_create.out fortax_verify_check.out

fortax_verify_create.out:fortax_verify_create.o $(FORTAXLIB)
	$(F90) $(FFLAGS) $< -o $@ $(FORTAXLIB)

fortax_verify_check.out:fortax_verify_check.o $(FORTAXLIB)
	$(F90) $(FFLAGS) $< -o $@ $(FORTAXLIB)

fortax_verify_create.o:fortax_verify_create.f90 $(FORTAXLIB)
	$(F90) $(FFLAGS) -c $< -I$(MODPATH)

fortax_verify_check.o:fortax_verify_check.f90 $(FORTAXLIB)
	$(F90) $(FFLAGS) -c $< -I$(MODPATH)

clean:
	rm -f *.mod *.out *.o
