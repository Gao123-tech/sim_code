function [Y, I, J] = sortvec(X)
%SORTVEC  performs vector sort using MATLAB's sort.m but also returns inverse indexing vector
%
%        Y = X(I)   ... sort also does this
%        X = Y(J)   ... sortvec adds this component
%
% sorry X can be vector only at this stage ...
%
% Amos L Maggi
% CMST, Physics
% Curtin University
%
% Revision 0       01-12-2003, ALM

if ndims(X) == 2 & min(size(X)) == 1
   if max(size(X)) == 1 % only scalar
      Y = X; I = 1; J = 1;
   else 
      [Y,I] = sort(X);
      [C,J] = sort(I);     
   end
end