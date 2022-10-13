function [filenames, pathname] = uigetfiles(extensions, fileprompt, pathprompt, catnames)
%uigetfiles        A shitty temporary replacement for uigetfile in multiple selection mode which (at least in 
%                  R14 and R14SP1) tends to exhibit <Cancel> response if if too many files are selected ...
%
%                  This is more awkward to use but using uigetfile piecewise is a tax if the files have to be sorted 
%                  other than by filename because this has to be repeated on each call ... too many mouse clicks
%
% USAGE:    [filenames, pathname] = uigetfiles(extensions, fileprompt, pathprompt);
%
% IP        see uigetfile
%
%           extensions   Rather like filerspec but only relevant to extensions ... format {[*].ext1, [*].ext2, ...}
%                        anything before the last '.' is stripped anyway 
%                        Extensions need to be explicit - no wildcards allowed
%                        [] or {} = *.*
%
%           pathprompt   prompt for path selection 
%                        '' = blank
%                        [] = causes default for uigetdir
%
%           fileprompt   prompt for path selection 
%                        '' = blank
%                        [] = causes default 'Select File' 
% 
%           catnames     logical - concatenates path and filenames (DEFAULT = FALSE)
%
% OP        filenames    cell vector of filenames ({} on Cancel)
%                        
%           pathname     (0 on Cancel)
%
%           
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0.0    16 December 2004 ... ALM 
%
% Revision 0.1    24 June     2005 ... ALM
%                 Change (temproray) multiselect so that uses single select to pick just one file in order to get path - 
%                 much more user friendly interface (can paste in path!)


%initialise
TRUE  = 1;
FALSE = 0;

filenames = {};
pathname  = '';
nfiles    = 0;

if ~exist('catnames', 'var'), catnames = FALSE; end

if ~exist('fileprompt', 'var'), fileprompt = []; end
if isnumeric(fileprompt)  % [] 
   fileprompt = 'Select Files(s) ... ';
end

if ~exist('pathprompt', 'var'), pathprompt = []; end
if isnumeric(pathprompt)  % [] is numeric
   %pathname = uigetdir(cd);
   [filename, pathname] = uigetfile(extensions, pathprompt);
else
   %pathname = uigetdir(cd, pathprompt);
   [filename, pathname] = uigetfile(extensions, 'Select path by selecting 1 file from it');
end

if isnumeric(pathname), return; end % CANCEL

% temporary change directory 
here = cd;
cd(pathname);

% get contents
d      = dir;


% apply filter if required from end, eliminating files as required
ntypes  = length(extensions);
nfiles  = 0;
ifile   = [];
if ntypes > 0
   for ii = length(d):-1:1,
      ext   = StripExtension(d(ii).name) ;
      found = FALSE                      ;
      jj = 1;
      while ~found && jj<=ntypes
         found = or(found, strcmpi(extensions{jj}, ext));
         jj = jj+1;
      end
      if found, 
         nfiles           = nfiles + 1   ;
         ifile            = [ifile, ii]  ;  
         filelist{nfiles} = d(ii).name   ;  % get names
      else
         d(ii)            = []           ;  % cull full list
      end
   end
   % reverse lists 
   if length(d) > 0
      ifile    = ifile(   end:-1:1);
      filelist = filelist(end:-1:1);
   end
else
   for ii = 1:length(d),
      filelist{ii} = d(ii).name ;  
   end
   ifile = [1:length(d)];
end

if length(d) <= 0, return; end

% display files for selections
[iout,OK] = listdlg('PromptString'  , fileprompt ,...
                    'SelectionMode' , 'multiple' ,...
                    'ListString'    , filelist       );
% create list
if ~OK 
   pathname = 0;
   % filenames is already empty
elseif length(iout) > 0
   filenames = filelist(iout);
   % concatenate names if reqd
   if catnames
      for ii = 1:length(iout)
         filenames{ii} = [pathname, '\', filenames{ii}] ;
      end
   end
end

cd(here);


