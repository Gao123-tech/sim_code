@ echo off

REM  Usage:back filename

erase PROJFIL TARGFIL ECHOFIL

copy %1.shd		PROJFIL  
copy %2.shd		TARGFIL  

c:\at\globalback

erase PROJFIL TARGFIL
rename SHDFIL			%3.shd 
