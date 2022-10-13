echo off

REM Usage: krakenc ENVFIL

if exist MODFIL0001 erase MODFIL0001
if exist %1.brc  copy %1.brc BRCFIL    
if exist %1.trc  copy %1.trc TRCFIL
if exist %1.irc  copy %1.irc IRCFIL

krakenc.exe < %1.env >	%1.prt

if exist BRCFIL erase BRCFIL
if exist TRCFIL erase TRCFIL
if exist IRCFIL erase IRCFIL
if exist %1.mod erase %1.mod
copy MODFIL0001		%1.mod

call field %1

