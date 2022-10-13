@ echo off

if exist SHDFIL erase SHDFIL

field3d.exe < %1.flp   

move SHDFIL	%1.shd	
move KBARFIL	%1.kbar	
move ZBARFIL	%1.zbar

toasc %1
