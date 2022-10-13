@ echo off

if exist SHDFIL erase SHDFIL

tobin.exe < %1.asc

REM Copy ascii file to scratch disk

move SHDFIL            %1.shd
