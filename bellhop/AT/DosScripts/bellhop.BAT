@ echo off

REM Usage: bellhop ENVFIL

if exist RAYFIL erase RAYFIL
if exist SHDFIL erase SHDFIL
if exist ARRFIL erase ARRFIL

if exist %1.brc  copy %1.brc BRCFIL
if exist %1.trc  copy %1.trc TRCFIL
if exist %1.bty  copy %1.bty BTYFIL
if exist %1.sbp  copy %1.sbp SBPFIL

bellhop.exe < %1.env > %1.prt

if exist RAYFIL  move RAYFIL	%1.ray
if exist SHDFIL  move SHDFIL	%1.shd
if exist ARRFIL  move ARRFIL    %1.arr
if exist BRCFIL  erase BRCFIL
if exist TRCFIL  erase TRCFIL
if exist BTYFIL  erase BTYFIL
if exist SBPFIL  erase SBPFIL

rem if exist %1.shd	toasc %1

