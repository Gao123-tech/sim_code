@ echo off

REM Usage: kraken ENVFIL

if exist MODFIL0001 erase MODFIL0001

kraken.exe < %1.env > %1.prt

if exist %1.mod erase %1.mod
copy MODFIL0001		%1.mod

field %1




