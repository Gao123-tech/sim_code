REM @ echo off

REM  Usage: stack foo
REM  needs foo.tim to specifyput times
REM  needs goo.fft to specify frequencies
REM  NOTE! name of the goo.fft file is specified in
REM  the *.tim input file

erase SHDFIL RTSFIL
copy %1.fft FFTFIL

%at\bb\stack < %1.tim

if exist SHDFIL  rename SHDFIL	%1.shd	
if exist RTSFIL  rename RTSFIL	%1.rts  

erase FFTFIL
