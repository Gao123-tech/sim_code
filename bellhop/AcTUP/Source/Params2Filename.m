function filename = Params2Filename(varargin)
%Params2Filename  Constrcuts filename (prefix) from basename and key parameters 
%                 based on ALM's extension of AJDs AcT convention for frequency labelling
%
% USAGE:          filename     =  Params2Filename(basename, id, val )
%                 filename     =  Params2Filename(basename, id1, val1, id2, val2, ... )
%
% INPUT:          basename     = base filename (no extension) string
%                 id#          = string idetifying for the parameter to be embedded in the filename
%                 val#         = parameter corresponding to be embedded after identifier id#
%
%                 "id" labels in use with AcT at time of writing this code include:
%                    ''   frequency (previously only the _ delimiter was used ... may be helpful to reserve 'f' also)
%                    'r'  cylindrical range
%                    'z'  depth
%
%                 Additional labels can be added. The token identifies are not limited to the above list 
%                 (or single characters) and should be controled programatically for consistency within 
%                 an application ... to ensure consistency, paramters should also be extracted using 
%                 "Filename2Params.m" 
%
% OUTPUT:         filename     = completed filename prefix ... paramters strings are concatenated to base
%                                name in the order provided
%
%
% EXAMPLES:       filename  =  [Params2Filename('yadayadayada', 'z', 15.5, 'r', 200), '.blah']
%                 ---> yadayadayada_z15.5_r200.blah
%
%
% PROG NOTES:     looks for p1 value in ascii string between '_[p1]' and the next '_' or '.'
%                 if more than 1 instance (that are numeric) then all are returned ... user will have to make a decision
%
%
% Related functions:   Filename2Params.m
%                 
%
% Revision History
% ================
% Revision 0.0    20 February 2004 ... ALM  
% Revision 0.1    21 April    2006 ... ALM  
%                 - add left zero padding for f data ONLY
% -----------------------------------------------------------------------------------------------------------------------


FALSE = 0;
TRUE  = 1;

% freq values only - pad to 10^5
fpadleft = 5; 

errhead = 'ERROR: Params2Filename -> ';

% checking i/p
if mod(nargin,2) ~= 1 || nargin <= 2
   error([errhead, 'i/o - invalid or incompatible number of input/output parameters']);
   return;
end
for ii = [1, 2:2:nargin-1]
   if ~ischar(varargin{ii})
      error([errhead, 'i/o - non-char input parameter detected in string input field']);
      return;
   end
end
for ii = 3:2:nargin
   if ~isnumeric(varargin{ii})
      error([errhead, 'i/o - non-numeric input parameter detected in numeric input field']);
      return;
   end
end

% initialise some stuff
filename = varargin{1};
% loop for each id/val pair
for ii = 2:2:nargin-1
   initok   = ['_', varargin{ii}];  % create initial delimiting token 
   num  = varargin{ii+1};
   nstr = num2str(num)  ;
   if strcmpi(varargin{ii}, 'f') || strcmpi(varargin{ii}, '')
       %pad with leading zeros to fpadleft to 10^5
       if num < 1,
           nstr(1) = [];
           nzeros = fpadleft-1;
       else
           nzeros = fpadleft - 1 - floor(log10(num));
       end
       for ii = 1:nzeros, nstr = ['0',nstr]; end;
   end
    
   filename = [filename, initok, nstr];
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%