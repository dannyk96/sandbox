

#  For Open MPI use, e.g., OMPI_FC=gfortran and for MPICH2, e.g., -f90=gfortran
export PATH := /usr/local/gfortran-bin/bin:$(PATH)
export LIBRARY_PATH := /usr/lib/x86_64-linux-gnu


FC= mpif90.mpich -fcoarray=lib
CAFLIB=-L /home/dan/git/opencoarrays/mpi -lcaf_mpi
.PHONY: clean

default: hw-coarray

APPS=  hello hello_s from_g95 hw-coarray
all: $(APPS)
hello_s: hello.f90
	gfortran -fcoarray=single -o $@ $^

hello: hello.f90
	$(FC) $< -o $@  $(CAFLIB) -lmpich

from_g95: from_g95.f90
	$(FC) $< -o $@  $(CAFLIB) -lmpich


hw-coarray: hw-coarray.f90
	$(FC) $< -o $@ $(CAFLIB) -lmpich 


clean:
	-rm $(APPS)
