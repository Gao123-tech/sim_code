echo off

copy %1.mod           MODFIL0001

field.exe < field.flp
move SHDFIL %1.shd
erase MODFIL0001
