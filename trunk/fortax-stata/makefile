FORTAX = ../modules-dev
FFLAGS = -O3 -fPIC -fpp -shared -no-prec-div -xHost
CFLAGS = -O3 -fPIC -Wall -Wno-write-strings -shared -DSYSTEM=OPUNIX
F90 = ifort
CPP = g++
OUTPUT = ~/ado/personal/fortax.plugin
CLIB = -L/opt/intel/Compiler/11.0/083/lib/intel64 -lifcore
FOBJ = fortax_sysdb.o fortax_stata.o

all:fortax_sysdb.o fortax_stata.o
	$(CPP) $(CFLAGS) fortax_plugin.cpp stplugin.c fortax_stata.o fortax_sysdb.o $(FORTAX)/fortax.a $(CLIB) -o $(OUTPUT)

fortax_sysdb.o:fortax_sysdb.f90 $(FORTAX)/fortax.a
	$(F90) $(FFLAGS) -c fortax_sysdb.f90 -I$(FORTAX)

fortax_stata.o:fortax_stata.f90 $(FORTAX)/fortax.a
	$(F90) $(FFLAGS) -c fortax_stata.f90 -I$(FORTAX)

clean:
	rm -f fortax_sysdb.o fortax_stata.o *.mod $(OUTPUT)

