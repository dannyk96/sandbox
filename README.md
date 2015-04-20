<script>
  $(document).ready(function () {
    $("article table").addClass("table table-condensed table-bordered table-striped");
  });
</script>
[TOC}

A collection of various tools
=============================
  Program |  Description 
|-----------------|---|
| Snow | _Snowflake fractals_ |
| Flow | _Flownet under a dam_ |

Roses are red 
Violets are blue

- [ ]  today
- [X] tomorrow 


#### Contents

######    6     *NULL_ARRAYS.
~~~
Storage at Gauss points =  8 *     128 GP's  =     0.004 Mb
Storage at Nodal points = 12 *     121 Nodes =     0.011 Mb
~~~   
######    7     *MAT_PROPS.
    1    model=2   E=1.e5  v=0.3  C=100.
    2    model=2   E=1.e5  v=0.3  C=100.    sprint=1


    1 Snow

This F90 program will generate a variety of line-fractals including the famous von-Koch Snowflake Curve.
However this is only as a test case. In Reality it is the prototype driver for enhancing [Danfront](https://github.com/dannyk96/danfe), an advancing front based unstructutred triangle mesh generator for finite elemeents.

    2 Stations

An f90 test of data structures

    3 flow
   
Flownet under dams  - used as an Undergraduate teaching example c. 1996   
