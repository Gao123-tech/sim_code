function    [vout1, vout2] = StripPath( fullname, optype )
% StripPath   seperates path and filename and returns both  
%
% usage:       pathname            = StripPath(fullname)
%              pathname            = StripPath(fullname, 'p')
%              filename            = StripPath(fullname, 'f')
%             [pathname, filename] = StripPath(fullname)      
%             
%
% This code starts looks for the rightmost '\' and returns:
%
% pathname -> everything to the left  of the identified '\' (inc '\')
% filename -> everything to the right of the identified '\'
%
% optype   -> 'f' return filename
%             'p' return pathname (default)
%
%              if nargout == 2, then pathname is always first
%              irrespective of optype
%
% e.g. > [pathname, filename] = StripPath('whitepages\bloggs\joe.dat')
%      > ans = 
%      >        pathname = 'whitepages\bloggs\'
%      >        filename = 'joe.dat'
%
% special cases:
%
% 1) if there is no '\', then the entire string is returned as a filename
%    and pathname = [];
%
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0.0  11 November 2003 - ALM
%


true  = 1;
false = 0;

% search for delimiter from right
[filename, pathname] = strtok(fullname(end:-1:1), '\');
pathname = pathname(end:-1:1);
filename = filename(end:-1:1);

% default assignment
vout1 = pathname;
% modificasi ? ...
if nargout <= 1
   if nargin > 1 & strcmpi(optype, 'f')
      vout1 = filename;
   end
else nargout == 2
   vout1 = pathname;
   vout2 = filename;
end

      
      
      
            
         
   