#!/bin/bash

echo " build the 2 programs:"
echo "   1. flow.f90   the analysis code"
echo "       This INCLUDEs solver.f90"
echo "   2. contour_ps.f90 "
echo "       This takes the .pl mesh file and the flow.out file and produces"
echo "       a postscript file called plotfile.ps"

echo  'To run you need a mesh in a file called <name>.pl'
echo  ' and also a small file with the material properties in called <name>.in'
echo  Note that currently this is hardcode as wall2.pl and wall2.in

gfortran -o contour_ps contour_ps.f90
gfortran -o flow flow.f90

echo
echo
echo " now run ./flow"
echo "the post-process with ./countour_ps"
#echo after running flow and subsequently contour_ps, try
B
#echo 'gs -sDEVICE=pdfwrite -sOutputFile="output.pdf" -dNOPAUSE -dEPSCrop -c "<</Orientation 2>> setpagedevice" -f "plotfile.ps" -c quit'
#echo or
#echo 'gs -dEPSCrop -c "<</Orientation 2>> setpagedevice" -f "plotfile.ps" -c quit'

