function ButtonStr = PlottingTools(Def, DirInfo, ExitFlags, ExitStrs)
%PlotTools       PlotTools for AcTUP ---------------------------------------------------------------
%
% USAGE:       ButtonStr = PlotTools(DirInfo, ExitFlags, ExitStrs);
%
% INPUT        DirInfo, Def --> See AcTUP
%
%              ExitFlags      = This is a vector of 17 boolean flags - one for each of the selectable operations excluding 'EXIT'
%
%                               'Amplitude-delay for each ray path', ...
%                               'Arrival path and timing',...
%                               'Bathymetry', ...
%                               'Environment', ...
%                               'Greens function', ...
%                               'Mode locations', ...
%                               'Mode shapes', ...
%                               'Rays', ...
%                               'Reflection coefficient', ...
%                               'Time series', ...
%                               'Time series as Fourier transform', ...
%                               'Transfer function', ...
%                               'Transfer function as impulse response', ...
%                               'Transmission loss vs depth', ...
%                               'Transmission loss vs range', ...
%                               'Transmission loss vs range and depth', ...
%                               'Transmission loss vs range @ multiple f', ...
%
%                               Note 'EXIT' is not selectable because ExitStr is always returned (since it ends the call anyway)
%
%                               When these flags are set TRUE, PostProcessor does not call these
%                               operations itself but exits (see Output) returning control to caller
%
%                               These are a list of functions that are normally available to PostProcessor as a standalone function
%                               But to which the PP's calling program may require restricted access or additional return control
%                               (i.e. other than via PP)
%
%                               [] or omit for autonomous operation
%
%              ExitStrs       = Cell array of strings which will appears as additional buttons and behave as as EXIT command
%                               If omitted or empty, the EXIT button string will be 'EXIT. These can be used to expand choices
%                               selected via PP but dealt with by calling program.
%
% OUTPUT       ButtonStr      = Button string that caused exit (see SetExitVec)
%
%
%
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      xx                ... AJD
%                   - Originally incorporated in AcToolboxFrontEnd.m
% Revision 1.0      10 June      2006 ... ALM (Viva l'Italia!!)
%                   - Parsed into function
%                   - Menu restructured somewhat
% Revision 1.1      25 August    2006 ... ALM 
%                   - Swap PlotBathymetry for subroutine with additional choices
% -----------------------------------------------------------------------------------------------------------
FALSE    = 0;
TRUE     = 1;

ReleaseSpec = LoadReleaseSpec();

BaseStrs = ...
  {
   'Amplitude-delay for each ray path', ...
   'Arrival path and timing',...
   'Bathymetries', ...
   'Environment', ...
   'Greens function', ...
   'Mode locations', ...
   'Mode shapes', ...
   'Rays', ...
   'Reflection coefficient', ...
   'Time series', ...
   'Time series as Fourier transform', ...
   'Transfer function', ...
   'Transfer function as impulse response', ...
   'Transmission loss vs depth', ...
   'Transmission loss vs range', ...
   'Transmission loss vs range and depth', ...
   'Transmission loss vs range @ multiple f', ...
   };

switch ReleaseSpec
   case 'LITE',
      BaseStrs(13:-1:10) = [];
      BaseStrs(2)        = [];
end
  
NBaseStrs = length(BaseStrs);

% INPUT CHECK
% -> exit flags can either be omitted, empty or the correct length
if ~exist('ExitFlags', 'var'), ExitFlags = []; end
if isempty(ExitFlags)
   ExitFlags = zeros(1, NBaseStrs);
elseif length(ExitFlags) ~= NBaseStrs
   error({'ERROR: PlotTools -> ExitFlags parameter is incorrect length', 'must be same length possible outputs'});
end
% -> Make sure there is at least one ExitStr
if ~exist('ExitStrs', 'var'), ExitStrs = []; end
if isempty(ExitStrs)
   ExitStrs = {'EXIT'};
end
% -> DirInfo
if ~exist('DirInfo', 'var'), DirInfo = []; end
if isempty(DirInfo)
   DirInfo = GetAcDirectoryInfo;
end
% -> Def
if ~exist('Def', 'var'), Def = []; end
if isempty(Def)
   %filename omitted to load default values
   Def = LoadRunDefinition(DirInfo);
end

% Build Menu adding Exit options
MenuStrs   = [BaseStrs, ExitStrs];
ExitFlags  = [ExitFlags, ones(1,length(ExitStrs))];

LaFine           = FALSE;
MainMenuStateIdx = 0    ;
StateIdx         = MainMenuStateIdx;

while ~LaFine
   % note that on return from sMainMenu, the switch loop will not be executed again if an ExitFlagged state was chosen
   switch StateIdx
      % =============================================================================================
      case MainMenuStateIdx
         [StateIdx, LaFine] = lMainMenu(MenuStrs, ExitFlags);
      % =============================================================================================
      otherwise
         State = BaseStrs{StateIdx};
         switch State
            case 'Amplitude-delay for each ray path'
               PlotArrivals([DirInfo.MainWork Def.SubDir]);
            case 'Arrival path and timing'
               PlotArrivalBounceTime([DirInfo.MainWork Def.SubDir]);
            case 'Bathymetries'
               lPlotBathymetries(DirInfo, Def);
            case 'Environment'
               PlotAcEnvArr(Def.EnvArr);
            case 'Greens function'
               PlotGreensFn([DirInfo.MainWork Def.SubDir]);
            case 'Mode locations'
               PlotModes([DirInfo.MainWork Def.SubDir]);
            case 'Mode shapes'
               PlotModeShapes([DirInfo.MainWork Def.SubDir]);
            case 'Rays'
               PlotRay([DirInfo.MainWork Def.SubDir]);
            case 'Reflection coefficient'
               PlotReflnCoefft([DirInfo.MainWork Def.SubDir]);
            case 'Time series'
               lPlotTS;
            case 'Time series as Fourier transform'                              
               lPlotFT;
            case 'Transfer function'
               lPlotTF;
            case 'Transfer function as impulse response'
               lPlotIR;               
            case 'Transmission loss vs depth'                
               PlotSlice([DirInfo.MainWork Def.SubDir], 'R');
            case 'Transmission loss vs range'                         
               PlotSlice([DirInfo.MainWork Def.SubDir], 'Z');
            case 'Transmission loss vs range and depth'
               PlotShade([DirInfo.MainWork Def.SubDir]);
            case 'Transmission loss vs range @ multiple f'
               lPlotTLvsRangeMultiF(Def, DirInfo);            
         end         
         StateIdx = MainMenuStateIdx; % this could vary for each Input State but not in this case
         % =============================================================================================
   end
end
%Return Last Button Press (can only be an Exit mode and can never be MainMenu)
ButtonStr = MenuStrs{StateIdx};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [StateIdx, LaFine] = lMainMenu(MenuStrs, ExitFlags)
FALSE    = 0;
TRUE     = 1;
LaFine   = FALSE;
StateIdx = menu( 'Plotting Tools', MenuStrs );
% Ditch switch/case for better programmatic control
% ... not as quick but this is isn't the slow part of acoustic modelling!
% Opt is now the index !
%State = MenuStrs{idx};
if ExitFlags(StateIdx); LaFine = TRUE; end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  lPlotBathymetries(DirInfo, Def)
% Add some choices

FALSE = 0;
TRUE  = 1;

% Build Menu adding Exit options
MenuStrs   = {'One or more bathymetry files', ...
              'Loaded bathymetry file', ...
              'Bathymetry files near loaded file', ...
              'Cancel'
             };

if isempty(Def.BathFName)
   State = MenuStrs{1};
else
   OptIdx = menu('Plot ...', MenuStrs);
   State  = MenuStrs{OptIdx};
end

if exist(DirInfo.Bath, 'dir')
   lookhere = DirInfo.Bath;
else
   lookhere = [DirInfo.MainWork Def.SubDir];
end

switch State
   case 'One or more bathymetry files'
      PlotBathymetries(lookhere);
   case 'Loaded bathymetry file'
      PlotBathymetries(Def.BathFName);
   case 'Bathymetry files near loaded file'
      PlotBathymetries(StripPath(Def.BathFName, 'p'));
end

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  lPlotFT
% Plots Fourier Transforms of cTimeSeries objects

FALSE = 0;
TRUE  = 1;

done = FALSE;
% read data file
here = pwd;
while ~done,
   [ts, tsname] = ReadTSFile('', 'TS', 'TS');
   if ~isempty(ts)
      PlotFourierTransform(ts);
   else
      done = TRUE;
   end
end
cd(here);

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  lPlotIR
% Plots Impulse Response (which is a TimeSeries, ok)

FALSE = 0;
TRUE  = 1;

done = FALSE;
% read data file
here = pwd;
while ~done,
   [H, tfname] = ReadTFFile('', 'TF', 'TF');
   if ~isempty(H)
      ir = ImpulseResponse(H);
      plot(ir);
   else
      done = TRUE;
   end
end
cd(here);

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  lPlotTF
% Plots Transfer Function
FALSE = 0;
TRUE  = 1;
done  = FALSE;
% read data file
here = pwd;
while ~done,
   [H, tfname] = ReadTFFile('', 'TF', 'TF')  ;
   if     ischar(H)
      % error message
      errordlg(H, 'PLOT Transfer Function');
   elseif ~isempty(H)
      plot(H)                              ;
   else
      done = TRUE                          ;
   end
end
cd(here)                                    ;

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  lPlotTS
% Plots TimeSeries
FALSE = 0;
TRUE  = 1;
done = FALSE;
% read data file
here = pwd;
while ~done,
   [ts, tsname] = ReadTSFile('', 'TS', 'TS');
   if ~isempty(ts)
      plot(ts);
   else
      done = TRUE;
   end
end
cd(here);

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lPlotTLvsRangeMultiF(Def, DirInfo)
%Multiple depth slices
rlim      = [1 1000];
nrows     = 1;
ncols     = 1;
lstyle    = 'r:';
Ans = inputdlg({'Sub directory', ...
                'File prefix', ...
                'Vector of frequencies', ...
                'Vector of figure numbers', ...
                'Hold current plots (y/n)', ...
                'Line style', ...
                'Range limits for plot (m)', ...
                'Number of subplot rows', ...
                'Number of subplot columns', ...
                'Subplot number for this plot' 
               },...
               'Files to be plotted:', ...
               [1,50], ...
               {Def.SubDir, Def.FPrefix, num2str(Def.Freq), int2str(Def.Freq), 'y', lstyle, num2str(rlim), int2str(nrows), int2str(ncols), '1'});
if ~isempty(Ans)
   PltDir = [DirInfo.MainWork Ans{1}];
   if PltDir(end) ~= '\'
      PltDir = [PltDir '\'];
   end

   FPrefix = Ans{2};
   PltFreq = str2num(Ans{3});
   FigVec = str2num(Ans{4});
   HoldCurrent = strcmpi(Ans{5}, 'y');
   LineStyle = Ans{6};
   RangeLimits = str2num(Ans{7});
   NRows = str2num(Ans{8});
   NCols = str2num(Ans{9});
   SubplNum = str2num(Ans{10});

   Ans = inputdlg({'Plot phase as well as magnitude (y/n)'}, '', [1,50], {'n'});
   if isempty(Ans)
      Status = 0;
   else
      PltPhase = strcmpi(Ans{1}, 'y');
      Status = 1;

      RxDepth = [];
      for IFreq = 1:length(PltFreq)
         %FBase = [FPrefix '_' num2str(PltFreq(IFreq))];
         FBase  = Params2Filename(FPrefix, '', PltFreq(IFreq));
         FigNum = FigVec(IFreq);

         if Status
            [PltDir, Status, RxDepth] = PlotSlice(PltDir, 'Z', FBase, FigNum, NRows, NCols, SubplNum, HoldCurrent, ...
               LineStyle, RangeLimits, PltPhase, RxDepth);
         end
      end
   end
end

         
% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%