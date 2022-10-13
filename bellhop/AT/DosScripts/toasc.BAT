@ echo off

if exist ASCFIL erase ASCFIL

copy/y %1.shd SHDFIL > foo.prt

REM Run the program

toasc.exe

REM Copy ascii file to scratch disk

copy/y ASCFIL		%1.asc > foo.prt
erase ASCFIL



