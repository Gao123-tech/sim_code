function NewDir = PlotBathymetries(DfltDir)
%PlotBathymetries          ... plots multiple bathymetry files
%
%Revision 0.0 ???
%
%Revision 0.1     25 August 2006 - ALM
%                 - test for filenames for immediate plot

persistent defans;

FALSE = 0;
TRUE  = 1;

if isempty(defans)
   defans = {'', 'y', '1 1', '1'};
end

fnames = {};
% MOD 0.1 input test
if exist('DfltDir', 'var')
   if ischar(DfltDir)
      if exist(DfltDir,'dir')
         NewDir = DfltDir;
      elseif exist(DfltDir,'file')
         fnames = {DfltDir};         
         NewDir = ''; % not needed
      else
         NewDir = pwd;
      end
   elseif iscell(DfltDir) 
      fnames = DfltDir;
      % test 1st at least
      if ~exist(fnames{1}, 'file')
         fnames = {};
         NewDir = StripPath(fnames{1}, 'p');
         if ~exist(NewDir,'dir')
            NewDir = pwd;
         end
      end
   end
end
Here = pwd;

if isempty(fnames)
   if nargin >= 1
      if exist(DfltDir, 'dir')
         cd(DfltDir);
      end
   end
   [fnames, pname] = uigetfile('*.bty', 'Select Bathymetry files to plot', 'MultiSelect', 'on');
   cd(Here);   
   if isnumeric(fnames)
      return;
   elseif ischar(fnames)
      fnames = {fnames};
   else
      fnames = sort(fnames);
   end
   % since input filenames could be from different directories, expand each of the ui selected files
   for ii = 1:length(fnames)
      fnames{ii} = [pname, fnames{ii}];
   end
end

done = FALSE;
while ~done
   [fhdls, current, next] = FindFigures;
   defans{1} = int2str(next);
   retans = inputdlg({'1st Figure number'        , ...
      'Hold existing plots (y/n)', ...
      'm x n plots'              , ...
      '# plots/axes'}            , ...
      'Bathymetry Plot Options', 1, defans);

   if isempty(retans)
      return;
   end
   fnum    = str2num(retans{1});
   holdon  = strcmpi(retans{2}, 'y') |  strcmpi(retans{1}, 'yes')  | strcmpi(retans{1}, '1') ;
   submat  = str2num(retans{3});
   nplt    = str2num(retans{4});
   if fnum < 1 | length(submat) ~= 2 | nplt <=0
      % keep asking
   else
      done = TRUE;
   end
end
% store for next default
defans   = retans;
lstyles  = LineStyleVector(nplt);
% when iplt reaches nplt, next sublot
iplt = 0;
% when isub reaches nsub, next figure
isub = 1;
nsub = submat(1)*submat(2);
for ii = 1:length(fnames)
   % sort out counters
   iplt=iplt+1;
   if iplt > nplt
      iplt = 1;
      isub = isub+1;
   end
   if isub > nsub
      isub = 1;
      fnum = fnum+1;
   end
   % extract data
   fid   = fopen(fnames{ii}, 'rt');
   NPt   = fscanf(fid, '%f', 1);
   InDat = fscanf(fid, '%f', [2, NPt]);
   Range = InDat(1, :)*1000;
   Depth = InDat(2, :);
   fclose(fid);
   % plot
   figure(fnum);
   subplot(submat(1),submat(2),  isub);
   if isub*iplt == 1 & ~holdon
      hold off;
   else
      hold on;
   end
   plot(Range, Depth, lstyles{iplt});
   ylim = get(gca, 'YLim');
   set(gca, 'YLim', [0, ylim(end)]);
   ylabel('Depth (m)');
   title(['Bathymetry - ' StripPath(fnames{ii}, 'f')], 'Interpreter', 'none');
   xlabel('Range (m)');
   view(0, -90);
end
