@ echo off

erase DATFIL COVFIL SHDFIL MODFIL0001

copy %1.shd		SHDFIL 
copy %2.mod		MODFIL0001  
set root = `basename %3`

c:\at\kraken\covar < %3.inp

rename DATFIL %root.dat
rename COVFIL %root.cov
erase SHDFIL MODFIL0001
