function DisplayShadeFileList(fnames, f, fsort)
%DisplayShadeFileList     cell array of shadefilenames in asceding f order
%                      
% USAGE:    DisplayShadeFileList(fnames, f, fsort)
%
% IP        fnames      cell array of shade filenames (1xm)
%           f           vector of frequencies ([])
%           fsort       == 0 do not sort 
%                       == 1 to sort by f  [DEFAULT]   
%
%           if omited, f and df are acquired using othert ShadeFile functions
%
% FOR NOW CODE ASSUMES ALL .shd FILES HAVE ONLY 1 f-COMPONENT 
%
% Revision 0.0    10 December 2004 ... ALM
%                 
% suggested mod: option to pass shdl type filename (test if ischar(fnames)) and open 

TRUE  = 1;
FALSE = 0;
len   = length(fnames);

if len <= 0, return; end

if ~exist('f', 'var'), f = []; end
if isempty(f) | length(f) ~= len
   for ii = 1:len
      [title, freq] = ReadShadeHeadBin(fnames{ii});
      f(ii)         = freq;
   end
end
if ~exist('fsort', 'var'), fsort = TRUE; end
if fsort
   [f, isort] = sort(f)      ;
   df         = diff(f)      ;
   fnames     = fnames(isort);
end

if fsort
   dispmsg{1} = sprintf('%s    %10.4f    %s', '----------', f(1), fnames{1});
   for ii = 2:length(fnames)
      dispmsg{ii} = sprintf('%10.4f    %10.4f    %s', df(ii-1), f(ii), fnames{ii});
   end
   PromptStr = 'df -> f -> Shade filename';
else
   for ii = 1:length(fnames)
      dispmsg{ii} = sprintf('%010.4f    %s', f(ii), fnames{ii});
      PromptStr = 'f -> Shade filename';
   end
end
   
listdlg('Name'         , 'ShadeFiles -> TF', ...
        'PromptString' , PromptStr,...
        'SelectionMode', 'single', ... % this is to remove <select all> button - maybe useful later
        'ListString'   , dispmsg, ...
        'ListSize'     , [600 400],...
        'OKString'     , 'DONE', ...
        'CancelString' , '');
