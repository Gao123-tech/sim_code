function yi = extrap1(x, y, xi, mode);
%EXTRAP1      Same as MATLAB's interp1 but will also do some crude extrapolation
%             INTERP1 is used in default (linear) mode only for now
%             
% yi = extrap1(x, y, xi 'mode');
%
% INPUT/OUTPUT
%      
%      x, y, xi, yi   - type help interp1 for a description of these vectors 
%                       x must be increase monotonically 
%
%      mode           - 'linear'  linear extrapolates using end and next-to-end points
%                     - 'last'    maintains nearest end point value
%                     - 'zero'    zero pads beyond end points 
%              
%
%	Centre for Marine Science and Technology
%
%   Revision: 1.0   Date: 02 September 1999 ALM
%
%             1.1         16 February  2000 ALM
%                   - improved algorithm for determining extrapolation limits
%
%             1.2         11 June      2001 ALM
%                   - add 'zero' option
%                   - fixed previously missed error-trap/message on low end extrapolation
%


n  = length(x);
ni = length(xi);
yi = ones(size(xi));

if ( xi(1) < x(1) )
  
  %need to extrapolate low
  
  imax = max(find(xi<x(1)));
  
  switch lower(mode)
  case 'linear'
    [x(1),y(1)] , [x(2), y(2)]
    [m,b] = grad_int([x(1),y(1)] , [x(2), y(2)]);
    yi(1:imax) = m.*xi(1:imax) + b;
  case 'last'
    yi(1:imax) = yi(1:imax) .* y(1);
  case 'zero'
    yi(1:imax) = yi(1:imax) .* 0;
  otherwise  
    disp('EXTRAP1: ERROR - invalid mode');
  end
  
else
  imax = 0;
end

if ( xi(ni) > x(n) )
  
  %need to extrapolate high
  
  imin = min(find(xi>x(n)));
  
  switch lower(mode)
  case 'linear'
    [m,b] = grad_int([x(n-1),y(n-1)] , [x(n), y(n)]);
    yi(imin:ni) = m.*xi(imin:ni) + b;
  case 'last'
    yi(imin:ni) = yi(imin:ni) .* y(n);
  case 'zero'
    yi(imin:ni) = yi(imin:ni) .* 0;
  otherwise  
    disp('EXTRAP1: ERROR - invalid mode');
  end
  
else
  imin = ni+1;
end

%now interpolate - if required

if imax+1 <= imin-1
  yi(imax+1:imin-1) = interp1(x,y,xi(imax+1:imin-1));
end


return;