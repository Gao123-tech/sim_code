@ echo off

REM Usage: bounce ENVFIL

if exist IRCFIL erase IRCFIL

bounce.exe < %1.env > %1.prt

move IRCFIL          %1.irc
move BRCFIL          %1.brc

