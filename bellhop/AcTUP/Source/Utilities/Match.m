function [match, miss] = Match(refvec, testvec, tol)
%Match   Compares contents of test vector to reference vector ... 
%
% USAGE                match         = Match(refvec, testvec[, tol])         ......... default
%                OR
%                      [match, miss] = Match(refvec, testvec[, tol])
%
% IP        refvec   = reference vector                                         1 x m or trans
%           testvec  = test vector                                              1 x n or trans
%           tol      = tolerance ([] or omit to use default = eps)
% OP
%           match    = indicies of common elements                              2 x r,  r:[0,n]
%                      1st row: vector of indicies of elements in testvec also found in refvec
%                      2nd row: vector of indicies of corresponding elements in refvec
%
%                      NOTES:
%                      a. these two vectors are equivilant:  
%                                                        > testvec(match(1,:)) == refvec(match(2,:));
%                  
%           miss     = indicies of unmatched elements in testvec                1 x (n-r)
%                      1st row: vector of indicies of elements in testvec NOT found in refvec
%           
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0        02 December 2003 ... ALM
%
% Revision 0.1      10 March    2005 ... ALM
%                   - add tolerance argument tol

%defs
FALSE   = 0;
TRUE    = 1;
TESTROW = 1;
REFROW  = 2;

%initialise
match   = [];
miss    = [];

if ~exist('tol', 'var')
   tol = [];
end
if isempty(tol)
   tol = eps;
end

ntest = length(testvec);

% ID match/miss loop
nmatch = 0;
nmiss  = 0;
for ii = 1:ntest,
   % look for a match
   matchidx = find( abs(refvec-testvec(ii)) < tol ); 
   % found ?
   if ~isempty(matchidx)  %    MATCH
      %increment counter and store matched indicies
      nmatch                 = nmatch + 1;
      match(TESTROW, nmatch) = ii;
      match(REFROW,  nmatch) = matchidx;
      matchidx               = [];      
   else                   % NO MATCH      
      %increment count and store index
      nmiss                  = nmiss + 1;
      miss(TESTROW, nmiss)   = ii;
   end
end