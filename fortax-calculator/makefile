F2PY = f2py
F90 = intelem
FORTAX = ../modules-dev
FFLAGS = -O3

all:returnbudcon.so UIgrid.py resources.py

returnbudcon.so:./f90/returnbudcon.f90  $(FORTAX)/fortax.a
	$(F2PY) --opt="$(FFLAGS)" -c -m returnbudcon --fcompiler=$(F90) ./f90/returnbudcon.f90 $(FORTAX)/fortax.a -I$(FORTAX)

UIgrid.py:./resources/UIgrid.ui
	pyuic4 ./resources/UIgrid.ui > UIgrid.py

resources.py:resources.qrc ./icons/copy.png ./icons/icon.png
	pyrcc4 -o resources.py resources.qrc

clean:
	rm -f $(OBJECTS) returnbudcon.so UIgrid.py resources.py myTable.pyc resources.pyc UIgrid.pyc
