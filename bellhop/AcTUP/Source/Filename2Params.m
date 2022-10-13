function [varargout] = Filename2Params(varargin)
%Filename2Params  Extracts key parameters from filename PREFIX 
%                 based on ALM's extension of AJDs AcT convention for frequency labelling
%
% USAGE:          val                 =  Filename2Params(filename, id )
%                 [val1, val2, ... ]  =  Filename2Params(filename, id1, id2, ... )
%
%          OR     [val1, basename]    =  Filename2Params(filename, id1)   
%
%                 In the last example, nargin = nargout = 2 and the last argout is the part of the file prefix before the '_' delimiter of id1
%
% INPUT:          filename     = filename (no extension) string
%                 id#          = string idetifying for the parameter embedded in the filename (see Params2Filename)
%
%                 Valid labels ('p1', 'p2', etc ..) used in AcT at time of writing this code include:
%                    ''   frequency (previously only the _ delimiter was used ... may also implement 'f'
%                    'r'  cylindrical range
%                    'z'  depth
%                 ... however more can be added. The token identifies are not limited to the above list 
%                 (or single characters) and should be controled programatically for consistency within 
%                 an application ... to ensure consistency, filenames should be created using 
%                 "Params2Filename.m" 
%
% OUTPUT:         val#         = recovered parameter corresponding to id#
%
%                 NOTE: if more than 1 instance (that are numeric) occur in any parameter, they are all returned ... 
%                       the user will have to make a decision
%
% EXAMPLES:       [z, r] = Filename2Params('yadayadayada_r123_z456', 'z')
%                      yields
%                            z = 456; r = 123;
%
%                 Try this one for yourself ...
%                      > filename          = 'yadayadayada_890_r1234_z567_y-111.1_1000_f999.0.blah';
%                      > [r, z, y, f1, f2] = Filename2Params(StripExtension(filename, 'p'), 'r', 'z', 'y', '', 'f')
%        
%
% PROG. NOTES:    Algorithm looks for val# in ascii string between ['_', id#] and the next instance of '_' (or eos)
%                 If more than 1 instance (that are numeric) then all are returned ... user will have to make a decision
%
%
% Related functions:   Params2Filename.m
%                 
%
% Revision History
% ================
% Revision 0.0    20 February 2004 ... ALM  
% -----------------------------------------------------------------------------------------------------------------------


FALSE = 0;
TRUE  = 1;

getbase =  FALSE;
errhead = 'ERROR: Filename2Params -> ';

%checking i/p
if nargin - nargout ~= 1 | nargin <= 1
   if nargin == nargout & nargin == 2
      %basename return option
      getbase = TRUE;
   else
      error([errhead, 'i/o - invalid or incompatible number of input/output parameters']);
      return;
   end
end
for ii = 1:nargin
   if ~ischar(varargin{ii})
      error([errhead, 'i/o - non-char input parameter detected']);
      return;
   end
end

filename = varargin{1};
iendbase = length(filename);

for ii = 2: nargin
   v      = [];
   vcount = 0 ;
   initok = ['_', varargin{ii}] ;    % for strfind (uses entire string token)
%  endtok = '_.'                ;    % for strtok  (uses any one character token) ... i.e. '_' OR '.'  
   endtok = '_'                 ;    % change: remove '.' and assume file prefix only has been passed so #s can now be non-integer

   % look for r flags
   iini = strfind(filename, initok);
   for jj = 1:length(iini)   % each start location
      % get characters between initial and end tokens
      substr = strtok(filename(iini(jj)+length(initok):end), endtok);
      % conver to a number
      vtemp = str2num(substr);
      % keep if the number is a string
      if ~isempty(vtemp)
         vcount = vcount + 1;
         v(vcount) = vtemp;
         iendbase  = min(iendbase, iini(jj) - 1);
      end
   end
   varargout{ii-1} = v;
   ilast           = ii;
end

if getbase
   varargout{ilast} = filename(1:iendbase);
end
   
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%