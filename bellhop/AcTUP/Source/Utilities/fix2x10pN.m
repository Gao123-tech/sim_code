function xout = fix2x10pN(x, fixpoints)
%fix2x10pN   Rounds TOWARDS ZERO from x to the nearest mulitple of fixpoints  and 10^n, where n is any + or - integer
%
%USAGE   xout = fix2x10pN(x, fixpoints)
%
%        fixpoints must be contain values in the range (1,10) and be in ascending order
%        a lower value of 1 is implied ... if it is not passed it is added to fixpoints
%
%
%        eg  fixto(0.0023, [2 2.5 5]) ->    0.002
%            fixto(6700  , [2 2.5 5]) -> 5000
%            fixto(1001  , [2 2.5 5]) -> 1000
%
% 
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0.0    25 February 2004 ... ALM

TRUE  = 1;
FALSE = 0;

xout = [];

% test input
if ~exist('x', 'var'),
   return;
end

if ~exist('fixpoints', 'var'),
   xout = fix(x);
   return;
end

if isempty(fixpoints)
   xout = fix(x);
   return;
else
   % force into a row vector
   fixpoints = reshape(fixpoints,1,length(fixpoints));
end

fixpoints = sort(fixpoints);

if fixpoints(1) < 1
   return;
end
if fixpoints(end) >= 10
   return;
end
if fixpoints(1) > 1
   fixpoints = [1, fixpoints];
end

% factorise
px = floor(log10(x));
fx = x * 10^-px;          % fx is always on [1,10)

done = FALSE            ;
ii   = 2                ;
n    = length(fixpoints);
while ~done   % loop for each member of fixpoints until fixed
   if     fx  <  fixpoints(ii)
      fx   = fixpoints(ii-1);
      done = TRUE           ;
   elseif ii  == n
      fx   = fixpoints(end) ;
      done = TRUE;
   end
   ii = ii+1;
end
% restore xout
xout = fx .* 10^px;






% looking back this can be confusing so i've kept the prototype hardcoded version for "convenient" integer division of 10

% EXAMPLE CODE FOR FOR fixpoints -> [1 2 2.5 5]
% if dz < 1m round dz down to either [1/10, 1/5, 1/4 or 1/2] m  ... or mulitples of 10^-n of these numbers
%                                  = [1     2    2.5 or 5  ] x 0.1m  "   " ...
% if dz > 1m round dz down to either [1     2    2.5    5  ] m  ... or mulitples of 10^+n of these numbers
%pdz = fix(log10(dz));
%fdz = dz * 10^-pdz;
%if     pdz < 1
%   % take out another factor of 10 to make the problem symmetrical
%  pdz = pdz -  1;
%   fdz = fdz * 10;
%end
% fdz can't be <  1 since that would cause pdz to be in the next integer range so ...
%if     fdz < 2
%   fdz = 1  ;
%elseif fdz < 2.5
%   fdz = 2  ;
%elseif fdz < 5
%   fdz = 2.5 ;
%elseif fdz < 10   % only remaining possiblity (again since pdz would change)
%   fdz = 5  ;
%end
% restore dz
%dz = fdz .* 10^pdz;
