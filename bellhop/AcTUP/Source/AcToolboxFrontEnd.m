function AcToolboxFrontEnd()
% AcToolboxFrontEnd
% User friendly matlab front-end for Mike Porter's Acoustics Toolbox
%
% Author:
% Alec J Duncan,
% Centre for Marine Science and Technology,
% Curtin University of Technology,
% Kent Street, Bentley, Western Australia
% a.duncan@cmst.curtin.edu.au
%
% Provides a uniform user interface for running and plotting the results from the following programs:
% kraken    (normal mode model)
% krakenc   (complex normal mode model)
% scooter   (fast-field model)
% bellhop   (gaussian beam tracing)
% bounce    (bottom reflection coefficient from geoacoustic model)
%
% This is especially handy for running models at multiple frequencies or for running different models on the same
% problem.
%
% The price for all this is, however, some loss of flexibility, as to keep the interface as simple as possible quite
% a lot of the parameters and options are hard-coded or computed by the program.
%
% This interface can only run kraken, krakenc and scooter for range independent problems.
% Bellhop can be run for range dependent bathymetry but not range dependent sound speed or bottom properties.
%
% The interface can only handle single source depths, but it can handle multiple receiver depths, multiple
% receiver ranges and multiple frequencies.
%
% If you get an obscure error message coming up in the command window check the appropriate .prn file for
% more meaningful error messages.  Consult the OALIB ...\doc\*.hlp files for further information.
%
% The Acoustic Toolbox is available from:
% ftp://oalib.saic.com/pub/oalib/AcousticsToolbox/
%
% Code has been tested with the version of the Acoustics Toolbox contained in:
%  atPII_f95.zip    June 27th 2002
%
% Works with Matlab 5 and Matlab 6 although Matlab 6 has a bug which makes some of the GUI windows sometimes only partly visible.
% Switching back and forth between windows usually solves the problem.
%
%*****************************************************************************************************************************
% To use it:
% 1. download and unzip the Acoustics Toolbox (atPII_f95.zip, see above) into a convenient directory
% 2. Unzip the AcToolboxFrontEnd archive into an appropriate directory and either
%    put this directory on your matlab path or cd to it from within matlab (don't put @AcEnvironment or @AcLayer on the path)
% 3. Use your matlab editor or another text editor to modify GetAcDirectoryInfo.m to specify the correct locations
%    for the Acoustic Toolbox files and the working directories you want to use.
% 4. At the Matlab command prompt, type act (this calls AcToolboxFrontEnd.m)
% 5. Hopefully from here on the menus and prompts will make it straightforward.
%
% The program starts up with a default run definition which you can run the various propagation models on as is, or
% you can modify the run definition to suit your own problems.  There are also a number of pre-defined run definitions that
% you can load, modify etc. - many of these are examples from "Computational ocean Acoustics" by Jensen, Kuperman,
% Porter and Schmidt.  Page numbers refer to the Springer 2000 edition - AIP Press edition page numbers are slightly different.
%
% You specify a directory and file prefix for each run and the program appends the frequency and an extension to make
% the file name.  Files with a number of different
% extensions are generated as follows:
%   .env     Environment file used when running the specified propagation program (all)
%   .prn     File used to log program output - check this if any errors occur (all)
%   .shd     Shade file, ie. acoustic transmission loss data (kraken, krakenc, scooter, RAM*, and bellhop
%            if told to compute transmission loss)
%   .shdl    List of .shd filenames (including extension) ...
%   .brc     Bottom reflection coefficient file - this in the format that bellhop likes (bounce and bellhop)
%   .ray     Ray file (bellhop if told to compute rays or eigenrays)
%   .arr     Amplitude/delay information for each arrival (bellhop if told to compute arraivals information)
%   .grn     Depth dependent Greens function (Scooter)
%   .mod     Mode file (kraken, krakenc)
%
% Input dialog boxes can handle valid matlab expressions as well as numeric values.  This is particularly useful
% for specifying  a vector of values, for example:
% 30:5:200
% specifies the vector [30, 35, 40, ... , 200]
%
% Sound speed profiles can either be specified manually or read from an ascii text file.
% Each row of file has following fields:
% Depth Cp Density Cs Ap  As
%
% where:
% Depth is depth below top of layer in metres
% Cp is compressional sound speed in m/s
% Density is density in kg/m^3
% Cs is shear sound speed in m/s
% Ap is compressional wave absorption in dB/wavelength
% As is shear wave absorption in dB/wavelength
%
% You don't have to specify all columns, any after the last specified will be set to defaults:
% Cp = 1500m/s, Density = 1024m/s, Cs, Ap, As = 0
%
% Bellhop can optionally be run with a bathymetry file for problems with bathynmetry dependent on range although the
% geoacoustic parameters and water column sound speed profile are treated as independent of range.
% The format of the bathymetry (.bty) file is as follows:
%   N
%   R(1) Zw(1)
%   R(2) Zw(2)
%    .     .
%    .     .
%    .     .
%   R(N)  Zw(N)
%
% Where: N is the number of range - depth pairs
%       R is the range in km
%       Zw is the water depth in metres
%
% Note that in this case the water column sound speed profile in the environment definition must be specified down to
% the deepest depth in the bathymetry out to the maximum range specified for the run.
%
% I've put a lot of error trapping in the code, but there may still be conditions that aren't trapped.  Please contact me
% at the above email address if you find any bugs or have any suggestions for improvements etc.
%
% **********************************************************************************************************
% Release notes:
% Changes incorporated in Version 1.1  (29 May 2002):
% - Added capability to read sound speed profiles from an ascii text file
%
% Changes incorporated in Version 1.2 (12th June 2002)
% - Fixed some bugs to do with default directories to make it easier to transfer run definitions
%   between machines
% - Fixed a bug in Greens function reading code (ReadGreen.m) that caused initial block of data to be
%   overwritten
% - Added act.m which simply calls actoolboxfrontend.m - for those who don't like typing!
% - Added additional error checking to prevent aborts on some types of directory creation errors
%   and aborts when trying to read invalid shade files
% - Added support for additional Bellhop run options
% - Added automatic scanning of log (.prn) files for error and warning messages
% - Number of mesh points in environment file is now set to zero so that propagation code will pick
%   a default value
% - Added option to allow manual editing of automatically generated environment files, .flp files etc.
%   prior to the code being run
%
% Changes incorporated in Version 1.3  (13th June 2002)
% - Code now reads binary shade files instead of converting to ascii
% - Added option to force exhaustive mode search when running Krakenc
% - Fixed problem with directory paths containing blanks
%
% Changes incorporated in Version 1.4 (14th June 2002)
% - Improved method of specifying layers so they automatically stack
% - Directory for each run definition is now specified relative to main working directory
% - added option to plot environment parameters (still a bit untidy)
%
% Changes incorporated in Version 1.5 (27th June 2002)
% - Fixed bug in error screening code that caused problems when there are a large number of warnings in .prn file
% - Code now consistently keeps intermediate files (bottom reflection coefft, mode, greens function)
% - Added capability to plot mode locations in complex Kr plane
% - Added capability to plot mode shapes
% - Added capability to plot amplitude - delay information produced by bellhop with 'A' option
% - Added screening for run type and beam type combinations not suppoprted by bellhop
%
% Changes incorporated in Version 1.6 (1st July 2002) - first public release
% - Added capability to read a run definition from a kraken or bellhop environment file
% - Tidied up environment plotting
%
% Changes incorporated in Version 1.7 (???) -
% - Mod to force reflection coefficient phase plot to -180 to +180 degree range
% - Fixed bug in PlotSlice so that phase is plotted correctly when requested
%
% Changes incorporated in Version 1.8 (XX February 2003) - ALM
% - RAMGeo added to propagation models
%
% Changes incorporated in Version 1.9 (10 November 2003) - ALM
% - add case - 'PltTLVRangeMultiFreq' (AJD)
% ~ RAM implementation (1.8) tested
% ~ .shdl file I/O introduced
% ~ Rx time series modelling introduced with two options
%      - shd files -> TF -> TS
%      - arr file  -------> TS
%
% Changes incorporated in Version 2.0  (5 August 2004) - AJD
% - added support for fully range dependent codes by using an array of environments
%
% Changes incorporated in Version 2.01 (from 14 October 2004) - ALM (not released)
% - RAMGeo implementation produces shade files now consistent with Scooter & Kraken
% - Rx time series modelling:
%      - shd files -> TF -> TS   actually works now
%      - TS, TF and IR plotting functions added to post-processor menu
%        (should eventualy move to plotting menu)
%
% Changes incorporated in Version 2.1 (November 2004) - ALM (STILL UNDERGOING MODS)
% - Add RAMSGeo
%      This is a CMST modification of RAMS which, on top of the changes required to work with AcT_UI,
%      modify give it the RAMGeo bathymetry model (RAMS uses the RAM b. model)
% - Change RunDef structure to accommodate and save ALL run parameters
% - Add batch file capability (May 2005)
%      This is a crude method first attempt - a better method is suggested
%      comments in LoadRunDefinition.m -> requires more work
% - Postprocessing additions (also as standalone): Micropath Modelling; 
%                                                  TransferFunctionEditor 
%                                                  TimeSeriesEditor
% - Some visualisation methods updated for better figure number control (can be extended to all ~FindFigures.m)
% - Additional visualition routine: Ray Arrival (interface bounces) overlaid on TimeSeries
%
% Version 2.2  29 June - 27 July 2006   ... ALM (viva l'Italia)
%              - Interface rehash
%                 >> seperation of environment and run definition functions
%                 >> seperation of model-specific run parameters
%                 >> regrouping of menu items
%                 >> streamlining to file handling and introduction of recoverable CurrentRunDef object file
%                 >> introduction of "l" release designation for limited or "lite" release
%             - Modifications to cope with  Mike Porter's 2006 release of Acoustic Toolbox
%                 >> SCOOTER
%                    ¤ .grn files: Greens Function 
%                      These now appear to have headers in the same format as shade files 
%                      ××  took a while to work this out but the fix was simple enough and 
%                          backward (read) compatibility maintained
%                 >> BELLHOP
%                    ¤ .arr files: Arrivals (amplitude-delay op data for A-mode run)
%                      For some reason the maximum number of rays for the identified arrival
%                      groups now listed in header (for each source depth) 
%                      ××  fixed and backward (read) compatibility maintained

% ALM's std initialisation
TRUE  = 1;
FALSE = 0;

% OTHER Initialisations
%     Version Number
VersionStr  = '2.2';
%     Menu Indicies - IMPORTANT THAT THESE POINT TO THE RIGHT BIT !!!
iSAMOpt  = 2;
iPostPro = 8;

DirInfo     = GetAcDirectoryInfo;
ReleaseSpec = LoadReleaseSpec();
DirInfo     = AddSourcePaths(DirInfo);

DefFName      = 'AcToolboxFrontEnd.mat';  %Name of file for storing defaults

%Reset the environment to default values
%Def = lLoadDefaultParams(DirInfo, DefFName);

%replace above with following call
Def = LoadRunDefinition(DirInfo, -1);



Done = 0;
NRows = 1;
NCols = 1;
SubplNum = 1;
PltFreq = Def.Freq;
RangeLimits = [0 1000];
LineStyle = 'r:';


SAMOpt   = BuildSAMOpt(Def.RunID);
MenuStrs = {'Configure Environment & Propagation Models'      , ...      % case  1
            SAMOpt                                            , ...      % case  2
            'Run Current Model for ACTIVE Propagation Code'   , ...      % case  3
            'Run Current Model for  ALL  Propagation Codes'   , ...      % case  4
            'Run Model(s) from File(s)'                       , ...      % case  5
            'View Log or Environment Files'                   , ...      % case  6
            'Plotting Tools'                                  , ...      % case  7
            'Post-Processor'                                  , ...      % case  8
            'Exit AcTUP'                                                 % case  9
           };                                   
   
%        REMOVED STATE:        State = 'ReadEnvFile';           

PostProExitStrs  = {'Main Menu', 'Exit AcTUP'}; 
switch ReleaseSpec 
   case 'LITE'
      TitleStr           = {'Acoustic Toolbox', 'User-interface', ['Version ' VersionStr, 'L']};
      MenuStrs(iPostPro) = [];
      PlotToolsExitStrs  = PostProExitStrs;
   case 'FULL'
      TitleStr           = {'Acoustic Toolbox', 'User-interface & Post-processor', ['Version ' VersionStr]};
      PlotToolsExitStrs  = [{'Post-Processor'}, PostProExitStrs];
      
end

State = 'Main Menu';
while ~Done
   switch State

      % =========================================================================================================================
      case 'Main Menu'         
         State = MenuStrs{menu(TitleStr, MenuStrs)};

      % =========================================================================================================================
      case 'Configure Environment & Propagation Models',

         [Def, DirInfo, State] = ConfigEnvironmentAndPropagation(Def, DirInfo, [], {'Main Menu'});
         MenuStrs{iSAMOpt}     = BuildSAMOpt(Def.RunID)          ;
         
         % -------------------------------------------------------------------------------------------------------------------------
      case MenuStrs{iSAMOpt}
               Def                = SelectActivePropagationCode(Def);
               MenuStrs{iSAMOpt}  = BuildSAMOpt(Def.RunID)          ;
               State              = 'Main Menu'                     ;
         % -------------------------------------------------------------------------------------------------------------------------
      case 'Exit AcTUP',
         Done = TRUE;

         %--------------------------------------------------------------------------------------------------------------------------
      case 'Plotting Tools',
         State = PlottingTools(Def, DirInfo, [], PlotToolsExitStrs);
         
         % ------------------------------------------------------------------------------------------------------------
      case 'Post-Processor'
         ExitFlags  = [0, ...%                                'SHD Files --> Transfer Function'
                       0, ...%                                'ARR File  --> Transfer Function'
                       0, ...%                                'ARR File  --> Rx Time Series'
                       0, ...%                                'TF        --> Rx Time Series'
                       0, ...%                                'µPath Modeller'
                       0, ...%                                'Time Series Editor'
                       0, ...%                                'Transfer Function Editor'
                       1 ];  %                                'Plotting Tools'
                  
         State = PostProcessor(DirInfo, ExitFlags, PostProExitStrs);

         %--------------------------------------------------------------------------------------------------------------------------
      case 'Run Current Model for ACTIVE Propagation Code',
         %Run Kraken, KrakenC, Scooter, etc.
         [Def, DirInfo] = lRunPropagationCode(Def, DirInfo);
         State = 'Main Menu';

         %--------------------------------------------------------------------------------------------------------------------------
      case 'Run Current Model for  ALL  Propagation Codes',
         %Run Kraken, KrakenC, Scooter, etc.
         [Def, DirInfo] = lRunPropagationCodes(Def, DirInfo);
         State = 'Main Menu';

         %--------------------------------------------------------------------------------------------------------------------------
      case 'Run Model(s) from File(s)',     %............................................................................... BATCH MODE
         DirInfo = lRunPropagationCodeBatch(Def, DirInfo);
         State = 'Main Menu';

         % ------------------------------------------------------------------------------------------------------------
      case 'View Log or Environment Files',
         ViewOpt = menu('File view options', ...
            'View log (.prn) file', ...
            'View environment (.env) file', ...
            'Main menu');
         switch ViewOpt
            case 1,
               State = 'ViewPrn';
            case 2,
               State = 'ViewEnv';
            case 3,
               State = 'Main Menu';
         end

         % ============================================================================================================
      case 'ReadEnvFile'
         Def = lReadEnvFile(DirInfo, Def);
         State = 'Main Menu';

      case 'TFViewDetails'               

         State = sTFViewDetails(DirInfo);

      case 'ViewPrn',
         lViewFile([DirInfo.MainWork Def.SubDir], '*.prn');
         State = 'View Log or Environment Files';

      case 'ViewEnv',
         lViewFile([DirInfo.MainWork Def.SubDir], '*.env');
         State = 'View Log or Environment Files';

   end   %switch State

end %while ~Done
RemoveSourcePaths(DirInfo);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lExecCmd(CmdStr, EchoCommands)    % THIS ONE'S COPIED STRAIGHT FROM RunPropgnCode
%Executes the specified dos command string
%This is done by writing the string to a batch file and then executing the batch file
%to avoid problems with matlab's dos shell not being able to handle blanks in directory names
%
% Revision 0.2  11 November 2003   ALM
%               - FID variable cleared on fclose
%               - fopen disabled if FID not <= 0

persistent Fid;

switch CmdStr
   case 'Reset',       % OPEN NEW batch file
      if isempty(Fid) | Fid <= 0, 
         Fid = fopen('CommandStr.bat', 'wt');
         if ~EchoCommands, fprintf(Fid, 'echo off\n'); end
      else
         disp('WARNING->RunPropgnCode: attempt to open new dos batch file before closing existing'); 
      end
      
   case 'Execute',     % CLOSE and EXECUTE batch file
      fclose(Fid);
      Fid = 0;
      !CommandStr.bat
      
   otherwise,          % WRITE Command string to batch file
      fprintf(Fid, '%s\n', CmdStr);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [Def, DirInfo] = lRunPropagationCode(Def, DirInfo, FromFile)
% INITIALISATION
FALSE      = 0;
TRUE       = 1;
APPENDFILE = FALSE;

% INPUT TEST
if ~exist('FromFile', 'var'), FromFile = FALSE; end

% RAM-specific final setup - this is historical - can change downstream usage of ...Species to ...RunID
% (e.g. in WriteToRAMFiles)
switch Def.RunID{1}
   case {'RAMGeo','RAMSGeo'}, 
      Def.RAM.Species = Def.RunID{1};
end

% Make sure the directory information is up to date
Def.Environment = SetDirInfo(Def.Environment, DirInfo);
Def.EnvArr      = SetDirInfo(Def.EnvArr, DirInfo);

% SET UP OP PATH & number of loops
% BUILD & TEST GROUP NAME & exit on fail
GroupName = BuildAndTestOutputDir(Def, DirInfo, ~FromFile);
if isempty(GroupName), return; end

% Slightly different approach for fields where we select all (for now) grn files in a particular directory
switch Def.RunID{1}
   case 'Fields'
      GrnFnames = dir([Def.Fields.GrnBasename, '_*.grn']);
      NFiles    = length(GrnFnames);
   otherwise
      NFiles    = length(Def.Freq);
end

% BEGIN CALC LOOP
tic
for ii = 1:NFiles
   switch Def.RunID{1}
      case 'Fields'
         f        = 0; % dummy
         % remove .grn extension ... this is automatically added later (via AcEnvironment->RunPropgnCode)
         BaseName = StripExtension([Def.Fields.GrnPathname, GrnFnames(ii).name],'p');
      otherwise
         f        = Def.Freq(ii);
         BaseName = Params2Filename(GroupName , '', f);
   end
   
   %Run the propagation model
   [Status, ErrStr] = RunPropgnCode(Def.EnvArr, Def, f, GroupName, BaseName, APPENDFILE, Def.RunID{1});
   toc
   if ~Status
      switch FromFile
         case FALSE
            disp('ERROR: AcTUP Run ->');
            disp(ErrStr)
         case TRUE
            uiwait(warndlg(ErrStr, 'ERROR: AcTUP Run ->', 'modal'), 1);
      end   
   end   
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function DirInfo = lRunPropagationCodeBatch(Def, DirInfo)
% Opens and runs a all RunDef objects stored in a specified folder

TRUE     = 1;
FALSE    = 0;
FromFile = TRUE;

% Open directory and pass all def files to sub
pname = uigetdir(DirInfo.LastBatch, 'Select Batch Folder - ALL RunDefinitions CONTAINED will be modelled');
if pname ~= 0 % not cancelled
   dfiles = dir([pname,'\*.mat']);
   if ~isempty(dfiles)
      DirInfo.LastBatch =  pname         ;
      dnames            = {dfiles.name}' ;
      bnum              = menu([num2str(length(dnames)), ' Run Definition Files ...'], ...
                               'RUN ALL', ...
                               'CANCEL'      );
      if bnum == 1
         donedir = [pname, '\done'];
         if exist(donedir, 'dir')
            isdoneisgood = TRUE;
         else
            isdoneisgood = mkdir(donedir);
         end
         for ii = 1:length(dnames);
            DefFile       = [pname,'\',dnames{ii}];
            [Def, FileOK] = LoadRunDefinition(DirInfo, DefFile);
            if FileOK
               [Def, DirInfo] = lRunPropagationCode(Def, DirInfo, FromFile);
               if isdoneisgood
                  movefile(DefFile, donedir);
               end
            end
         end
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [Def, DirInfo] = lRunPropagationCodes(Def, DirInfo)
% Cycles through all of the COMPLETE Propagation Codes - mainly DEBUG/TEST TOOL
% By Complete I mean Bounce+Bellhop and not bounce or bellhop only as well  .. same with Scooter+Fields

Codes = {'Bounce+Bellhop', ...
         'Kraken'        , ...
         'KrakenC'       , ...
         'RAMGeo'        , ...
         'RAMSGeo'       , ...
         'Scooter+Fields'};
%
for ii = 1:length(Codes)
   Def.RunID = Codes(ii)                         ;  % pass cell element not string content!
   [Def, DirInfo] = lRunPropagationCode(Def, DirInfo) ;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  Def = lReadEnvFile(DirInfo, Def);

ThisDir = DirInfo.MainWork;
Here = pwd;
Status = 1;
if ~exist(ThisDir, 'dir')
   Status = LibMakeDirectory(ThisDir);
end

if  Status
   cd(ThisDir);
end

[File, Path] = uigetfile('*.env', 'Environment file to load');
cd(Here);

if File ~= 0
   [EnvObj, RunInfo] = LoadFromEnvFile(Def.Environment, [Path File]);
   if ~isempty(RunInfo)
      Def.Title = GetName(EnvObj);
      Def.Freq = RunInfo.Freq;
      Def.Environment = EnvObj;
      %/* ###
      EnvArrInfo.Name = 'Environment array from .env file';
      EnvArrInfo.EnvArr{1} = EnvObj;
      EnvArrInfo.RangeVec = 0;
      EnvArrInfo.dRInterp = 0;
      Def.EnvArr = AcEnvArr(EnvArrInfo, DirInfo);
      %*/

      Def.Zs = RunInfo.Zs;
      Def.Zr = RunInfo.Zr;
      if ~isempty(RunInfo.RMin)
         Def.RMin = RunInfo.RMin;
         Def.RMax = RunInfo.RMax;
         Def.NRange = RunInfo.NRange;
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function NewDir = lViewFile(DefDir, Mask);
Here = pwd;
cd(DefDir);
[File, Path] = uigetfile(Mask, 'File to view');
cd(Here);

if File ~= 0
   NewDir = Path;
   CmdStr = ['!notepad ' Path File];
   eval(CmdStr);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
