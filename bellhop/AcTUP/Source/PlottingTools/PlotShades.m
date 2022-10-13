function NewDir = PlotShades(DefaultDir)
%PlotShades()     Select and plots mulitple shade files
%
%Revision 0.0   10 June 2005 ... ALM
%               - Based on AJD's modification of Mike Porter's PlotShade function (from version 3.1)

persistent defans;
persistent pname;

FALSE = 0;
TRUE  = 1;

if isempty(defans)
   defans = {'', '1 1', '60', 'n'};
end
[fhdls, current, next] = FindFigures;
defans{1} = int2str(next);

if exist('DfltDir','dir')
   NewDir = DfltDir;
elseif isdir(pname)
else
   NewDir = pname;
end
Here = pwd;

if nargin >= 1
   if exist(DfltDir, 'dir')
      cd(DfltDir);
   end
end
[fnames, pname] = uigetfile('*.shd', 'Select Shade files to plot', 'MultiSelect', 'on');
cd(Here);

if isnumeric(fnames)
   return;
elseif ischar(fnames)
   fnames = {fnames};
else
   fnames = sort(fnames);
end

done = FALSE;
while ~done
   retans = inputdlg({'1st Figure number'        , ...
                      'm x n plots'              , ...
                      'Plot dynamic range (dB)', ...
                      'Remove cylindrical spreading (y/n)'}, ...
                      'Bathymetry Plot Options', 1, defans);

   if isempty(retans)
      return;
   end
   fnum              = str2num(retans{1})      ;
   submat            = str2num(retans{2})      ;
   DynRange          = str2num(retans{3})      ;
   RemoveCylindrical = strcmpi(retans{4}, 'y') ;

   if fnum < 1 | length(submat) ~= 2 
      % keep asking
   else
      done = TRUE;
   end
end

% store for next default
defans   = retans;
% when iplt reaches nplt, next sublot
iplt = 0;
% when isub reaches nsub, next figure
isub = 1;
nsub = submat(1)*submat(2);
for ii = 1:length(fnames)
   
   %START EXTRACT from PlotShade.m ----------------------------
   filename = [pname fnames{ii}];
   % extract data
   Fid = fopen( filename, 'rb' );
   Status = 1;
   if Fid < 0
       uiwait(warndlg(['Couldn''t open input file: ' filename]));
       Status = 0;
   end

   if Status
      % Pre-read the file to determine what frequencies are present
      FVec = [];
      FileStatus = 1;
      while FileStatus
         [ pltitl, freq, nsd, nrd, nrr, sd, rd, rr, tlt, FileStatus ] = ReadShadeBin( Fid );
         if FileStatus
            FVec = [FVec freq];
         end
      end
      if isempty(FVec)
         Status = 0;
      else
         if length(FVec) > 1
            if length(FVec) < 20
               Prompt = {['Choose frequency to plot: ' num2str(FVec)]};
            else
               Prompt = {['Choose frequency to plot: ' num2str(FVec(1)) ' to ' num2str(FVec(end))]};
            end
            Ans = inputdlg(Prompt, '', 1, {num2str(FVec(1))});

            if isempty(Ans)
               Status = 0;
            else
               ThisFreq = str2num(Ans{1});
               [Dummy, IFreq] = min(abs(FVec - ThisFreq));
            end
         else
            IFreq = 1;
         end
         if Status
            frewind(Fid);
            for ISub = 1:IFreq
               [ pltitl, freq, nsd, nrd, nrr, sd, rd, rr, tlt, Status ] = ReadShadeBin( Fid );
            end
         end
      end
      fclose(Fid);
   end

   if Status
      isd = 1;  %index of source to be plotted
      if nrd <= 1
         msgbox(...
            'Propagation code must be run with multiple receiver depths in order for field to be plotted', ...
            'PlotShade:', 'warn');
         return;
      end

      zt = rd;
      taker = 1:nrr;
      rt = rr( taker );


      %Ans = inputdlg({'Figure number', 'Plot dynamic range (dB)', ...
      %        'Remove cylindrical spreading (y/n)'}, '', 1, {int2str(gcf), '60', 'n'});

      % sort out counters
      if isub > nsub
         isub = 1;
         fnum = fnum+1;
      end
      % plot
      figure(fnum);
      subplot(submat(1),submat(2),  isub);
      isub = isub+1;
      
      %alec's graph setup removed to not interfere with MATLAB 7 
%      if exist('GUI_SetupGraphMenu.m', 'file')
%         GUI_SetupGraphMenu('pcolor');
%      end

      TL = -20*log10(abs(tlt));
      if RemoveCylindrical
         TL = TL - ones( nrd, 1 ) * 10.0 * log10( rt )';
      end

      PltTLMin = floor(min(min(TL))/5)*5;  %Minimum transmission loss for plot - round to nearest 5 dB
      PltTLMax = PltTLMin + DynRange;

      pcolor( rt, zt, TL );
      shading interp;
      caxis([PltTLMin PltTLMax]);
      colormap('default');
      Map = colormap;
      colormap(flipud(Map));  %Want small TL = big signal = red

      h = colorbar;
      h2 = get(h, 'ylabel');
      if RemoveCylindrical
         set(h2, 'string', 'Transmission Loss - 10logR (dB)');
      else
         set(h2, 'string', 'Transmission Loss (dB)');
      end
      view( 0, -90 );
      xlabel( 'Range (m)' ); ylabel( 'Depth (m)' );
      title( [deblank( pltitl ) '. f=' num2str(freq) 'Hz, Zs=' num2str(sd(isd)) 'm.'] , ...
         'interpreter', 'none');
   end
end