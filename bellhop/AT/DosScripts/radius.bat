REM @ echo off

erase SHDFIL SHDFILIN

copy %1:r.shd		SHDFILIN		

REM  Run the program

c:\at\global\radius 

REM  Copy ascii file to scratch disk

rename SHDFIL            %1:rR.shd	
erase SHDFILIN

REM 5.0,			! SIGMA
REM 100.0, 50.0, 101,	! FOMMIN, FOMMAX, NFOM

REM 8.0,			! SIGMA
REM 100.0, 60.0, 41,	! FOMMIN, FOMMAX, NFOM
