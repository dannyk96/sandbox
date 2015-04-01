#!/bin/bash

echo build the 2 programs
echo    1. flow.f90   the analysis code
echo         This INCLUDEs solver.f90
echo    2. contour_ps.f90 
echo         This takes the .pl mesh file and the flow.out file and produces
echo         a postscript file called plotfile.ps

echo  'To run you need a mesh in a file called <name>.pl'
echo  ' and also a small file with the material properties in called <name>.in'

gfortran -o contour_ps contour_ps.f90
gfortran -o flow flow.f90

echo try
echo            How many Contour bands do you want (eg. 20)
echo 'gs -sDEVICE=pdfwrite -sOutputFile="output.pdf" -dNOPAUSE -dEPSCrop -c "<</Orientation 2>> setpagedevice" -f "plotfile.ps" -c quit'
echo or
echo 'gs -dEPSCrop -c "<</Orientation 2>> setpagedevice" -f "plotfile.ps" -c quit'

