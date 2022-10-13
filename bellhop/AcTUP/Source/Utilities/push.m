function n = push(x)
%PUSH   Round away from zero.
%    PUSH(X) rounds the elements of X to the nearest integers
%    away from zero (opposite direction to FIX(X)).
% 
%    See also FIX, FLOOR, ROUND, CEIL
%
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 29        04 April 2004 ... ALM

fixx = fix(x);
n = fixx + sign(x)*ceil(abs(x-fixx));