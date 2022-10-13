@ echo off

modbin.exe < %1.moA

move MODFIL %1.mod
erase %1.moA
