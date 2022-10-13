REM @ echo off

copy %1:r.shd		SHDFILIN		

REM  Run the program

c:\at\global\pod 

REM  Copy ascii file to scratch disk

rename SHDFIL            %1:rP.shd	
erase SHDFILIN

REM  80.0  5.0		! FOM, SIGMA
