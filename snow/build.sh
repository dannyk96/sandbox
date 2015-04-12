#!/bin/bash
#
#  Build script for snow.exe  
#  Too small to bother with a makefile :-)
#  Dan Kidger  31/03/15
#  daniel.kidger@gmail.com
#
rm *.o *.mod
FC=$(FC)
$(FC) -c -g segments.f90
$(FC) -cpp -DPGPLOT -c -g plot.f90
$(FC) -c -g snow.f90
echo 'note that we do not use afront.f90 (in a future release)'
$(FC) -o snow snow.o segments.o plot.o -L$HOME/pgplot -lpgplot -lX11

echo try this:
echo 'echo -e "/XWIN\n3\n2\n.4\n3\n\n\n" | ./snow'

# notes on Salfrod FTN95
# ftn95 segments.f90; ftn95 /CFPP /DEFINE SALFORD  1 plot.f90 ; ftn95 snow.f90 /LINK  




