function [NewDir, Status, RxSlice] = PlotSlice(DefaultDir, Mode, FBase, FigNum, NRows, NCols, SubplNum, ...
   HoldCurrent, Style, OrdinateLimits, PltPhase, RxSlice)
% PlotSlice()
% plots a single TL slice from the shade file
%
% INPUT:          Mode              =  'Z' for depth slice [DEFAULT]
%                                   OR 'R' for range slice
%                                   Mode must be passed if other parameters besides DefaultDir are passed
%
%                 OrdinateLimits    Limits for plotting [previously RangeLimits]
%                 RxSlice           Slice at THIS Receiver Depth or Range depending on Mode [previously RxRange]
%
% Modified by AJD 13/6/02 to read binary shade file
% Modded by Alec Duncan to support files containing data from multiple frequencies 6/9/02
%
% Revision 0.1      26 November 2004 - ALM
%                   - add optional "Mode" parameter to cope with range slice
%                   - some IP parameter labels changed to avoid confusion
%
% Revision 0.2      08 March    2005 - ALM
%                   - fix bad code in ~ phase plot (i screwed it up last time)
%
% Revision 0.3      04 April    2005 - ALM
%                   - change default parameters to better avoid accidental overwite and change phase plot limits
%
% Revision 0.31     04 April    2005 - ALM
%                   - change to make use of (new) ReadShadeBin EOF flag 


%constants
TRUE  = 1;
FALSE = 0;

%DefaultDir
Status = 1;
if nargin > 2
   filename = [DefaultDir FBase '.shd'];
   NewDir = DefaultDir;
   GotFileName = 1;
   GotOrdinateLimits = 1;
else
   GotOrdinateLimits = 0;
   if nargin < 1
      ChangeDir = 0;
      Mode = 'Z';
   elseif nargin < 2
      Mode = 'Z';
   else
      StartDir = pwd;
      DirStat = 1;
      if ~exist(DefaultDir, 'dir')
         DirStat = LibMakeDirectory(DefaultDir);
      end
      if DirStat
         cd(DefaultDir);
         ChangeDir = 1;
      else
         ChangeDir = 0;
      end
   end

   [FName, Path] = uigetfile('*.shd', 'Select file to plot');

   if ChangeDir
      cd(StartDir);
   end

   if FName == 0
      NewDir = DefaultDir;
      Status = 0;
   else
      Status = 1;

      filename = [Path FName];
      NewDir = Path;
   end
end

% change mode flag to boolean
ZSlice = [];
if ischar(Mode)
   switch upper(Mode(1))
      case 'Z'
         ZSlice = TRUE;
      case 'R'
         ZSlice = FALSE;
   end
end
if isempty(ZSlice)
   % write to command line and dialog box
   disp('ERROR: PlotSlice - > Illegal parameter value for "Mode"');
   errordlg('Illegal parameter value for "Mode"', 'ERROR: PlotSlice', 0);
   return;
end

if Status
   Fid = fopen( filename, 'rb' );
   if Fid < 0
      uiwait(warndlg(['Couldn''t open input file: ' filename]));
      Status = 0;
   end
end

if Status
   % Pre-read the file to determine what frequencies are present
   FVec = [];
   FileStatus = 1;
   EOF        = 0;
   while FileStatus && ~EOF
      [ pltitl, freq, nsd, nrd, nrr, sd, rd, rr, tlt, FileStatus, EOF ] = ReadShadeBin( Fid );
      if FileStatus
         FVec = [FVec freq];
      end
      
   end
   if isempty(FVec)
      Status = 0;
   else
      if length(FVec) > 1
         if length(FVec) < 10
            Prompt = {['Choose frequency to plot: ' num2str(FVec)]};
         else
            % this next line looks like crap - may need to recode - possible listbox with single selection option or similar
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

if Status;
   isd = 1;  %Source number to plot

   if Status

      Range = rr;
      if ZSlice
         Ordinates = rr;
         SliceThru = rd;
         Ostr      = 'Range';
         Sstr      = 'Depth';
         Slabel    = 'Zr'   ;
      else
         Ordinates = rd;
         SliceThru = rr;
         Ostr      = 'Depth';
         Sstr      = 'Range';
         Slabel    = 'Rr'   ;
      end

      if nrd > 1
         if (nargin < 12) | ((nargin >= 12) & isempty(RxSlice))
            Ans = inputdlg({['Enter desired ', Sstr, ' (m) [', num2str(SliceThru(1)) ' - ' num2str(SliceThru(end)), ']']}, ...
               'PLotSlice', 1, {num2str(SliceThru(1))});
            if isempty(Ans)
               return;
            end
            RxSlice = str2num(Ans{1});
         end
         [MinDiff, idx] = min(abs(RxSlice - SliceThru));
      else
         idx = 1;
         RxSlice = SliceThru(1);
      end

      if nargin == 1 | nargin == 2
         [fhdls, current, next] = FindFigures();
         Ans = inputdlg({...
            'Figure number', ...
            'Hold current plot (y/n)', ...
            'Line style', ...
            'Plot phase as well as TL (y/n)'}, '', 1, {int2str(next), 'y', 'b-', 'y'});
         if isempty(Ans)
            return;
         end

         FigNum = str2num(Ans{1});
         HoldCurrent = strcmpi(Ans{2}, 'y');
         Style = Ans{3};
         PltPhase = strcmpi(Ans{4}, 'y');
         NCols = 1;
         NRows = 1;
         SubplNum = 1;

         if PltPhase
            Ans = inputdlg({'Unwrap 360 degree phase jumps? (y/n)'}, '', 1, {'n'});
            if ~isempty(Ans)
               UnwrapPhase = strcmpi(Ans{1}, 'y');
            else
               return
            end
         end

      else
         UnwrapPhase = 0;
      end

      figure(FigNum);
      if ~HoldCurrent
         if exist('GUI_SetupGraphMenu.m', 'file')
            GUI_SetupGraphMenu('plot');
         end
      end
      if PltPhase
         if ZSlice
            subplot(2,1,1);
         else
            subplot(1,2,1);
         end
      else
         subplot(NRows, NCols, SubplNum);
      end

      if HoldCurrent
         hold on;
      else
         hold off;
      end

      if ZSlice
         plot( Ordinates, -20*log10(abs(tlt( idx, : ))), Style)
         if GotOrdinateLimits
            axis([OrdinateLimits(1) OrdinateLimits(2) -inf inf]);
         end
         xlab = [Ostr, ' (m)'];
         ylab = 'Transmission Loss (dB)';
      else
         plot( -20*log10(abs(tlt( :, idx ))), Ordinates, Style)
         if GotOrdinateLimits
            axis([-inf inf OrdinateLimits(1) OrdinateLimits(2)]);
         end
         ylab = [Ostr, ' (m)'];
         xlab = 'Transmission Loss (dB)';

      end
      view( 0, -90 );
      set(gca, 'Box', 'on');
      xlabel( xlab ); ylabel( ylab );
      title( [deblank( pltitl ) ': f=' num2str(freq) 'Hz, Zs=' num2str(sd(isd)) 'm, ',Slabel, '=', num2str(SliceThru(idx)) 'm'] , ...
         'interpreter', 'none');

      if PltPhase
         if ZSlice
            PrComplex = tlt(idx, :).';
         else
            PrComplex = tlt(:, idx).';
         end
         if UnwrapPhase
            Phase = unwrap(angle( PrComplex));
         else
            Phase = angle( PrComplex);
         end

         if ZSlice
            ahdl = subplot(2,1,2);
         else
            ahdl = subplot(1,2,2);
         end
         if HoldCurrent
            hold on;
         else
            hold off;
         end

         if ZSlice
            phdl = plot(Ordinates, Phase, Style);
            if GotOrdinateLimits
               axis([OrdinateLimits(1) OrdinateLimits(2) -inf inf]);
            end
            if ~UnwrapPhase
              set(ahdl, 'YDir'      , 'reverse');  
              set(ahdl, 'YLim'      , [-pi, pi]); 
              set(ahdl, 'YTick'     , [ -pi , -pi/2 , 0 , pi/2 , pi ]); 
              set(ahdl, 'YTickLabel', {'-pi' '-pi/2' '0' 'pi/2' 'pi'});
            end
            xlab = [Ostr,' (m)'];
            ylab = 'Phase (rad)';
         else
            phdl = plot(Phase, Ordinates, Style);
            if GotOrdinateLimits
               axis([-inf inf OrdinateLimits(1) OrdinateLimits(2)]);
            end
            if ~UnwrapPhase
              set(ahdl, 'XLim'      , [-pi, pi]); 
              set(ahdl, 'XTick'     , [ -pi , -pi/2 , 0 , pi/2 , pi ]); 
              set(ahdl, 'XTickLabel', {'-pi' '-pi/2' '0' 'pi/2' 'pi'});
            end
            ylab = [Ostr,' (m)'];
            xlab = 'Phase (rad)';
         end
      end
      view( 0, -90 );
      set(gca, 'Box', 'on')
      xlabel( xlab );
      ylabel( ylab );
   end
end
