@ echo off

copy %1.mod		MODFIL

modasc.exe > %1.moA

erase MODFIL
