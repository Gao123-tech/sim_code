   function xout = roundto10pN(xin, n)
%roundto10pN   Rounds to nearest decimal place specified (N<0 right of decimal place)
%
%    See also push, fix2x10pN.m
%
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0        14 March 2005 ... ALM

gain = 10^-n;
xout = round(xin.*gain)./gain;