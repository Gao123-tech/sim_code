REM

erase INFIL
copy %1:r.shd		INFIL		

REM Run the program

c:\at\global\switchsr 

rename SHDFIL            %1:r.shd		
erase INFIL


