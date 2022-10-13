function Profile = GetProfile(Layer, Type, WithZ)
% Returns the specified profile for the specified layer
%
%    WithZ = true  (1)  - Profile = 2 x N with Z vector as first row vector
%    WithZ = false (1)  - Profile = 1 x N 
%
%    Type string options   - 'Rho'
%                            'Cp'
%                            'Cs'
%                            'Ap'
%                            'As'
%                            'Z'  (WithZ ignored for this property - always row vector)
%          
%   returns -1 for invalid Type string
%
% Revision 0.0    05 June 2003 - ALM
% Rev 1.1   6 Aug 2004 - AJD  - mods so it works irrespective of whether data is in rows or columns

Profile = [];

if nargin < 3
   WithZ = false;
end

NZ = length(Layer.Z);

if WithZ | Type == 'Z'
      Z = reshape(Layer.Z, 1, NZ);
      Profile = [Profile; Z];
end


% there's a neater way of doing this but this will do 
switch Type
   case 'Rho'
      Rho = reshape(Layer.Rho, 1, NZ);
      Profile = [Profile; Rho];
   case 'Cp'
      Cp = reshape(Layer.Cp, 1, NZ);
      Profile = [Profile; Cp];
   case 'Cs'
      Cs = reshape(Layer.Cs, 1, NZ);
      Profile = [Profile; Cs];
   case 'Ap'
      Ap = reshape(Layer.Ap, 1, NZ);
      Profile = [Profile; Ap];
   case 'As'
      As = reshape(Layer.As, 1, NZ);
      Profile = [Profile; As];
   case 'Z'
      % already covered
   otherwise
      Profile = -1;
end



