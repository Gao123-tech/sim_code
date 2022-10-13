function [Xnew, inew] = MinimiseLength(X)
%MinimiseLength   Minimises length of a sequence of values by keeping only 1st and last instance in a string or repeats (N > 2) 
% 
% USAGE:     [Xnew, inew] = MinimiseLength(X)
%
% IP          X        = sequence          
%
% OP          Xnew     = new sequence
%             inew     : Xnew = X(inew);
% 
%
% Revision History
% ================
% Revision 0.0    06 02 2006 ... ALM  
% -----------------------------------------------------------------------------------------------------------------------

% i'm sure there's a more elegant way of doing this but this will do for now

FALSE  = 0        ;
TRUE   = 1        ;

% go
nX     = length(X);
first  = 1        ;
last   = 0        ;
icut   = []       ;
nrun   = 1        ;
for ii = 2:nX,
   if X(ii) == X(first)
      nrun = nrun + 1;
   else
      if nrun > 2 % was there enough elements in the last run to execute cut
         icut = [icut, first+1:ii-2];
      end
      first = ii   ;
      nrun  = 1    ;
   end
end
if nrun > 2 % was there enough elements in the last run to execute cut
   icut = [icut, first+1:ii-1];
end
inew       = 1:nX    ;
inew(icut) = []      ;
Xnew       = X(inew) ;



      
      
   