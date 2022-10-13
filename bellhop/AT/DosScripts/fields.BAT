@ echo off

if exist SHDFIL erase SHDFIL
copy/y %1.grn		GRNFIL

fields.exe < fields.flp

move SHDFIL	%1.shd
erase GRNFIL

rem toasc %1

