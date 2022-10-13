@ echo off

REM Usage: sparc ENVFIL

if exist GRNFIL erase GRNFIL
if exist RTSFIL erase RTSFIL
if exist %1.sts  copy %1.sts		STSFIL

REM Run SPARC

sparc.exe < %1.env

if exist GRNFIL  move GRNFIL  %1.grn  
if exist RTSFIL  move RTSFIL  %1.rts
if exist STSFIL  erase STSFIL


