function [match, nearest, approx] = MatchOrFindNearest(refvec, testvec, tol)
%MatchOrFindNearest   Compares contents of test vector to reference vector ... 
%
% USAGE                match                     = MatchOrFindNearest(refvec, testvec[, tol])
%                OR
%                      [match, nearest]          = MatchOrFindNearest(refvec, testvec[, tol])
%                OR
%                      [match, nearest, newvec]  = MatchOrFindNearest(refvec, testvec[, tol])
%
% IP        refvec   = reference vector                                                           1 x m or trans
%           testvec  = test vector                                                                1 x n or trans
%           tol      = tolerance ([] or omit to use default = eps)
% OP
%           match    = indicies of common elements                                                2 x r,  r:[0,n]
%                      1st row: vector of indicies of elements in testvec also found in refvec
%                      2nd row: vector of indicies of corresponding elements in refvec
%
%                      NOTES:
%                      a. these two vectors are equivilant:  
%                                                        > testvec(match(1,:)) == refvec(match(2,:));
%                      b. if there is degeneracy in refvec, only the first matched instance is identified
%                         where the order in refvec is as supplied (see c & d)
%                  
%           nearest  = indicies of unmatched and corresponding closest matching elements          2 x (n-r)
%                      1st row: vector of indicies of elements in testvec NOT found in refvec
%                      2nd row: vector of indicies of elements in refvec closest to the unmatched values 
%           
%                      MORE NOTES:
%                      c. refvec is reordered using SortVec prior to searching for nearest values 
%                      d. the returned nearest and matched indicies for the refvector are those in the 
%                         unsorted vector (as supplied) NOT the sorted vector
%                      e. if a test value falls midway between two reference values, the lower VALUE is indexed
%                         as nearest (that is the value with a lower index in the sorted reference vector)
%
%           approx   = indexes both matched and nearest values in refvec                          1 x n
%                      a "nearest" approximation to testvec can be created in one step using:
%                                                        > newvec = refvec(approx); 
% 
% Function calls include: 
%                        find_nearest
%                        Match
%                        SortVec
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
nearest = [];
newvec  = [];
if ~exist('tol', 'var')
   tol = [];
end
if isempty(tol)
   tol = eps;
end

% run match first
[match, nearest] = Match(refvec, testvec, tol);

% look for nearest match if required
nmiss = length(nearest);
if nargout > 1 & nmiss > 0
   % sort .arr range in case this vector does not increase monotonically - required for find_nearest.m
   sortedref = SortVec(refvec);
   
   for ii = 1:nmiss,
      % find nearest value and its index in sorted reference vector
      nearestvalue       = find_nearest( sortedref, testvec( nearest(TESTROW, ii) ) );
      % find position of nearest value in the unsorted reference vector
      refidx             = find( abs(refvec-nearestvalue) < tol );
      % there could be more than 1 occurance, but there will be at least 1 by construction ... take 1st
      nearest(REFROW,ii) = refidx(1);  
   end
   matchidx = [];
end

% create replacement/nearest vector if required
if nargout > 2
   % join and sort by 1st row 
   approx = sortrows([match, nearest]', 1)';
   % 1st row is redundant
   approx(1,:) = [];
end

