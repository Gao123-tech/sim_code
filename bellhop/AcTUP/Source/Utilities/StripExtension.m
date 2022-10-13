function    [vout1, vout2] = StripExtension( fullname, optype )
% StripExtension   seperates path and filename and returns both  
%
% usage:       extension           = StripExtension(fullname)
%              extension           = StripExtension(fullname, 'x')
%              prefix              = StripExtension(fullname, 'p')         ...includes path if present
%             [extension, prefix]  = StripExtension(fullname)      
%             
%
% This code starts looks for the rightmost '.' and returns:
%
% extension -> everything to the right of the identified '.' (including '.')
% prefix    -> everything to the left  of the identified '.' (excluding '.')
%
% optype   -> 'x' return extension (default)
%             'p' return prefix 
%
%              if nargout == 2, then extension is always first
%              irrespective of optype
%
% e.g. > [extension, prefix] = StripExtension('whitepages\bloggs\joe.dat')
%      > ans = 
%      >        extension = '.dat'
%      >        prefix    = 'whitepages\bloggs\joe'
%
% special cases:
%
% 1) if there is no '.', then the entire string is returned as a prefix
%    and extension = [];
%
% SEE ALSO StripPath.m
%
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0.0  19 November 2003 ... ALM
% Revision 0.01 16 December 2004 ... ALM
%               - BUG FIX ... filename '.' no longer crashes routine 
%              

true  = 1;
false = 0;

% search for delimiter from right
[extension, prefix] = strtok(fullname(end:-1:1), '.');
% check output

% BUG FIX 0.01 - check prefix length
if length(prefix) == 0 & length(extension) == 0 
   extension = fullname;
elseif ~strcmp(extension(end:-1:1), fullname)  % token found
   % move dot from prefix to extension
   extension = [prefix(1) extension(end:-1:1)];
   prefix    = prefix(end:-1:2);
else                             % no '.' 
   % put it all into prefix
   prefix    = extension(end:-1:1);
   extension = [];
end
 

% default assignment
vout1 = extension;
% modificasi ? ...
if nargout <= 1
   if nargin > 1 & strcmpi(optype, 'p')
      vout1 = prefix;
   end
else nargout == 2
   vout1 = extension;
   vout2 = prefix;
end

      
      
      
            
         
   