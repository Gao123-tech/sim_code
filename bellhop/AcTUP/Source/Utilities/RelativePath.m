function relpath = RelativePath(abspath, refpath);
%RelativePath    converts path to path relative to reference path
%
%USAGE   relpath = RelativePath(abspath, refpath)
%        relpath = RelativePath(abspath)          % uses current directory as reference
%
%        relpath -> NOT OK (relative path not possible) -> EMPTY
%                -> OK                                  -> never(!) ends in path delimiter (\ or /) unless end is up tree:  e.g. '../' 
%                                                          (so safe to pass filesnames at end of abspath)
%                                                       -> any leading path delim is removed 
% 
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0.0 - 16 February 2006 ... ALM

OS_UNIX = 0;
OS_WIN  = 1;
FALSE   = 0;
TRUE    = 1;

OS      = OS_WIN;

switch OS
   case OS_UNIX
      delim = '/';
   case OS_WIN
      delim = '\';
end

if ~exist('refpath', 'var')
   refpath = cd;
elseif isempty(refpath)
   refpath = cd;
end
   
% remove trailing '\' if it exists
if abspath(end) == '\';
   abspath(end) =   [];
end
if refpath(end) == '\';
   refpath(end) =   [];
end

% compare path components from front to find common section 
done         = FALSE;
[abit, arem] = strtok(abspath, delim);
[rbit, rrem] = strtok(refpath, delim);
relpath      = '';
if ~strcmpi(abit,rbit)
   % first bit doesn't match so use of relative path impossible
   done = TRUE;
end
% loop from 2nd element if any
while ~done
   
   if length(rrem) == 0          % relative path exhausted
      % relative path is the remainder of the absolute path
      relpath = arem;
      done    = TRUE;
   elseif length(arem) == 0      % absolute path exhausted
      % relative path is a series of "ups" of the same length as the remainder (:length = number of sub levels)
      [subs, nsubs] = strtoks(rrem, delim);
      for ii = 1:nsubs
         relpath   =  [relpath, '..', delim];
      end
      done    = TRUE;
   else                          % neither path exhausted
      [abit, arem] = strtok(arem, delim);
      [rbit, rrem] = strtok(rrem, delim);
      if ~strcmpi(abit,rbit)     % mismatch
         % back up to previous dir
         [subs, nsubs] = strtoks(rrem, delim);
         for ii = 1:nsubs+1
            relpath   =  [relpath, '..', delim];
         end
         % add absolute path remnant from this mismatched sub
         relpath = [relpath, abit, arem];
         done = TRUE;
      else
         % another bit of the directory structure is rejected (i.e. rems has been overwritten)
      end
   end
end
if ~isempty(relpath)
   if relpath(1)     == delim
      relpath(1)      = [];
   end
%   if relpath(end)   ~= delim
%      relpath(end+1)  = delim;
%   end
end
