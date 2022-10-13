function cw = cw_ClayMedwin1977(T, S, z);
%cw_ClayMedwin1977    generates sound speed profile in water given -> ref: Clay and Medwin (1977), "Acoustical Oceanography"
%
%                     cw = 1449.2 + 4.6.*T - 0.055.*T.^2 + 0.00029.*T.^3 + (1.34-0.01.*T).*(S-35) + 0.016.*z
%
%USAGE            cprofile = cw_ClayMedwin1977(T, S, z);
%
%INPUT            T        = temperature             [°C]   n x 1 OR 1 x 1
%                 S        = salinity                [ppt]  n x 1 OR 1 x 1
%                 z        = depth                   [m]    n x 1 OR 1 x 1
%
%OUTPUT           cw       = compressive sound speed [m/s]  n x 1 
%                         OR error mssage on failure (string)
%
%                 NOTES -> vector orientation doesn't matter but should be consistent for fwd compatibility (will not generate matrix o/p yet)
%                       -> o/p will be a column vector 
%                       -> Australia is pretty much surrounded by S ~ 35 ppt
%
%
% Centre for Marine Science & Technology, 
% Physics Dept
% Curtin University
% Western Australia
%
% Revision 0.0   22 March    2005  ... ALM

% initialise
TRUE    = 1;
FALSE   = 0;
errhead = 'ERROR: cw_ClayMedwin1977 -> ';

% lengths
nT = length(T);
nS = length(S);
nz = length(z);
n  = max([nT nS nz]);
% test for vectors
if nT ~= prod(size(T))
   cw = [errhead 'temperature (T) not a vector or scalar'];
   return;
end
if nS ~= prod(size(S))
   cw = [errhead 'salinity (S) not a vector or scalar'];
   return;
end
if nz ~= prod(size(z))
   cw = [errhead 'depth (z) not a vector or scalar'];
   return;
end

if n == 1
   % no more to do to IP
else
   if nT > 1 & nT < n
      cw = [errhead 'inconsistent IP vector lengths'];
      return;
   elseif nT == n
      T = reshape(T, n, 1);
   end
   if nS > 1 & nS < n
      cw = [errhead 'inconsistent IP vector lengths'];
      return;
   elseif nS == n
      S = reshape(S, n, 1);
   end
   if nz > 1 & nz < n
      cw = [errhead 'inconsistent IP vector lengths'];
      return;
   elseif nz == n
      z = reshape(z, n, 1);
   end
end

cw = 1449.2 + 4.6.*T - 0.055.*T.^2 + 0.00029.*T.^3 + (1.34-0.01.*T).*(S-35) + 0.016.*z;