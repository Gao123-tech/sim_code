@ echo off

REM Usage: scooter ENVFIL

if exist %1.brc  copy %1.brc BRCFIL    
if exist %1.trc  copy %1.trc TRCFIL
if exist %1.irc  copy %1.irc IRCFIL
if exist GRNFIL erase GRNFIL

scooter.exe < %1.env > %1.prt

move GRNFIL          %1.grn
if exist BRCFIL erase BRCFIL
if exist TRCFIL erase TRCFIL
if exist IRCFIL erase IRCFIL

call fields %1
