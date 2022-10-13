@ echo off

@ ctr = 1
foreach modfil ( %argv[*] 
   erase MODFIL%ctr
   copy %modfil.mod           MODFIL%ctr  
   @ ctr = %ctr + 1
end

c:\at\kraken\fieldmo < fieldmo.flp

erase MODFIL*
rename SHDFIL		%1.shd	

