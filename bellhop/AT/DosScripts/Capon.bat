@ echo off

REM  Usage: capon REPFIL COVFIL

erase REPFIL COVFIL SHDFIL

copy %1.shd		REPFIL  
copy %2.cov		COVFIL  

c:\at\global\capon

erase REPFIL COVFIL
rename SHDFIL		%2C.shd 
