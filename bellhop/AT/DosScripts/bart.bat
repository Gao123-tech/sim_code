@ echo off

REM  Usage: bart REPFIL COVFIL

erase REPFIL COVFIL SHDFIL

copy %1.shd		REPFIL  
copy %2.cov		COVFIL  

c:\at\global\bart

erase REPFIL COVFIL
rename SHDFIL		%2B.shd 
