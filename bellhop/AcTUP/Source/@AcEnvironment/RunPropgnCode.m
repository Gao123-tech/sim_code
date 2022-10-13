function [Status, ErrStr] = RunPropgnCode(Env, RunDef, Freq, GName, FName, AppendFile, PropgnCode, EchoCommands)
% RunPropgnCode  Creates the necessary input files to run, and runs, the specified acousitc propagation code
%                For available functions see PropgnCode argument description below
%
% USAGE:     [Status, ErrStr] = RunPropgnCode(Env, RunDef, Freq, GName, FName, AppendFile, PropgnCode, EchoCommands);
%
% RunDef     - (expanded) Run Definition Strucure
%
% Freq       - frequency (Hz)
%
% GName      - group filename
%              -> (relative) path + prefix only (no extension)
%              -> routine will write (append) the filname (sans path) of the .shd
%                 file (if generated) to the list in this file (GName.shdl)
%              -> successive filenames (FName.shd) are LF delilimited
%              -> to disable pass 0 ... DO NOT OMIT
% FName      - file name for output file including path but excluding extension.
%              -> the following files may be generated (internally or externally) and renamed ...
%                   FName.arr    amplitude delay file
%                   FName.asc    ascii shade file ?? (prob before AJD realised how big these would be and how many he would need)
%                   FName.brc    bottom reflection coefficient file (bounce)
%                   FName.env    actual environment file used by Scooter
%                                actual input file used by RAM*
%                                -> all these formats are independent although the intra-RAM versions are similar
%                   FName.flp    input file used by Fields
%                   FName.grid   STD  RAM o/p file: transmission loss magnitude            (copied from tl.grid) [bin]
%                   FName.in     standard input file for RAM (implemented as ram*.in)
%                   FName.line   STD  RAM o/p file: transmission loss magnitude @ Rx depth (copied from tl.line) [asc]
%                   FName.pgrid  CMST RAM o/p file: complex pressure                       (copied from  p.grid) [bin]
%                   FName.pline  CMST RAM o/p file: complex pressure @ Rx depth            (copied from  p.line) [asc]
%                   FName.prn    file to log program messages
%                   FName.shd    complex transmission loss file [bin]
%
% AppendFile - 1 to append .asc file to an existing file of the same name, 0 to create new file
%              -> NOTE this is currently ignored when RAM is being used
%
% Pre version 1.1
% PropgnCode - determines acoustic propagation code to be run:
%              -> 'Bellhop'    Gaussian bean tracing program - requires additional information in ExtraParams structure
%              -> 'Bounce'     Seabed reflection coefficient calculation
%              -> 'Kraken'     Normal mode code
%              -> 'KrakenC'    Complex arithmetic normal mode code
%              -> 'RAM'        Parabolic equation (PE) code
%                              (RAMGeo only at this stage)
%              -> 'Scooter'    Fast-field program
%
% Version 1.1
% PropgnCode - determines acoustic propagation code to be run:
%              -> 'Bellhop'         Gaussian bean tracing program - requires additional information in ExtraParams structure
%              -> 'Bounce'          Seabed reflection coefficient calculation
%              -> 'Fields'          Inversion of Green's Function solution by FFT
%              -> 'Kraken'          Normal mode code
%              -> 'KrakenC'         Complex arithmetic normal mode code
%              -> 'Scooter'         Green's Function solver
%              -> 'Scooter+Fields'  Complete Fast-Field solutions
%
% --------------------------------------------------------------------------------------------------------------------------------
% Alec J Duncan,
% Centre for Marine Science and Technology,
% Curtin University of Technology,
% Kent Street, Bentley, Western Australia
% a.duncan@cmst.curtin.edu.au
% --------------------------------------------------------------------------------------------------------------------------------
%
% Revision History
% ================
% Revision 0    ?? May      2002   AJD
% Revision 0.1  26 May      2003   ALM
%               - Add RAM(Geo)
% Revision 0.2  11 November 2003   ALM
%               - add GName to i/p list and .shdl functionality to accom' Transfer Function at field point post-proc.
%               - reformatted and cleaned up this header ~;]
%               - modified code to save both STD RAM output and CMST RAM complex pressure output (bug in v0.1)
%
% Revision 0.3  17 March    2004   ALM / AJD
%               - modified to handle new WriteToKrakenFiles.m call (additional parameters wrt previous version).
%                (the call was copied from a parallel [working, non-RAM] version maintained by Alec ...)
%
% Revision 0.4  12 October  2004   ALM
%               - modified RAM* sections to be more flexible to changes in RAM executable
%                 (uses GetRAMVersionData.m)
%
% Revision 1.0  03 December 2004   ALM
%               - uses expanded RunDef structure ...
%               - paramter list not compatible with previous versions
%
% Revision 1.1  12 July     2006   ALM
%               - uses reorganised Propagation Code IDs (better seperation between Scooter and Fields)
%               - future mods could include seperation between Kraken* & Field
%               - Already redundant RAM code now deleted
%                 See AcEnvArr.RunPRopgnCode for latest code if required to be transplanted in
%
% Revision 1.2 21 July     2006   ALM
%               - 'Bellhop' is really -> 'Bounce+Bellhop' and therefpore changed 
%                 (In future may have Bellhop on its own for using bounce results - not yet)
%               - BUG FIX ... Moved reset command for batch file to inside loops (remember 'execute' also closes file)
%               - UPDATE  ... Looks like Kraken/KrakenC zero pad their mode file numbers ... and not all upper case 
%                             (AT 2006 release probably)
%                             Change 'MODFIL1' <-- 'MODFil0001'
%                      


% POTENTIALLY VERSION-DEPENDENT SETUP PARAMETERS ----------------------------------------------------------------------------
NARGINMAX = 10;
% ---------------------------------------------------------------------------------------------------------------------------

UsePltrth = 0;    %1 to compute bottom reflection coefft using plotrth.exe, 0 to use BRCFILE generated directly by bounce.exe

Status = 1;
if nargin < NARGINMAX
   EchoCommands = 1;
end
ErrStr = 'OK';

% open environment file
switch PropgnCode
   case 'Fields'
      % no environment object is passed
   otherwise
      EnvFile = fopen([FName '.env'], 'wt');
      if EnvFile <= 0
         Status = 0;
         ErrStr = ['couldn''t open environment file: '  FName '.env'];
      end
end

% open extra files as (and if) required by particular model
if ~Status
   return;
end

switch PropgnCode
   case {'Kraken', 'KrakenC', 'Scooter+Fields', 'Fields'},
      FlpFile = fopen([FName '.flp'], 'wt');
      if FlpFile <= 0
         Status = 0;
         ErrStr = ['couldn''t open .flp file: '  FName '.flp'];
      end
   case 'Bounce+Bellhop'
      EnvFile2 = fopen([FName '_2.env'], 'wt');
      if EnvFile2 <= 0
         Status = 0;
         ErrStr = ['couldn''t open .env file: '  FName '2.env'];
      end
   case 'Scooter',
      FlpFile = [];
end % switch

if ~Status
   return;
end

% WRITE SETUP FILES FOR PROPAGATION CODES
switch PropgnCode

   case 'Bounce+Bellhop',
      WriteToBounceFiles(Env, Freq, EnvFile, RunDef.Title, UsePltrth);  %Need to run bounce to get bottom reflection coefft
      fclose(EnvFile);
      WriteToBellhopFiles(Env, Freq, RunDef.RMin, RunDef.RMax, RunDef.dR,  RunDef.Zr, RunDef.Zs, EnvFile2, RunDef.Bellhop, RunDef.Title);
      fclose(EnvFile2);

      if RunDef.ManualEnvEdit
         lEditFile([FName '.env']);
         lEditFile([FName '_2.env']);
      end

   case 'Bounce',
      WriteToBounceFiles(Env, Freq, EnvFile, RunDef.Title, UsePltrth);
      fclose(EnvFile);
      if RunDef.ManualEnvEdit
         lEditFile([FName '.env']);
      end

   case {'Fields'}
      %Set up the required input files
      WriteToFieldsFiles(Env, FlpFile, RunDef.Fields.rmin, RunDef.Fields.rmax, RunDef.Fields.nr)
      fclose(FlpFile);
      if RunDef.ManualEnvEdit
         lEditFile([FName '.flp']);
      end

   case {'Kraken', 'KrakenC'},
      switch PropgnCode
         case 'Kraken'
            RunDef.Kraken.ExhaustiveSearch =0;
      end

      WriteToKrakenFiles(Env, Freq, RunDef.RMin, RunDef.RMax, RunDef.dR,  RunDef.Zr, RunDef.Zs, EnvFile, FlpFile, RunDef.Title, ...
         RunDef.Kraken.ExhaustiveSearch);
      fclose(FlpFile);
      fclose(EnvFile);
      if RunDef.ManualEnvEdit
         lEditFile([FName '.env']);
         lEditFile([FName '.flp']);
      end

   case {'RAM'}
      % create array object to access the modifed WriteToRamFiles
      %EnvArrData.Name     = '';
      %EnvArrData.EnvArr   = {Env};
      %EnvArrData.RangeVec = 0;
      %EnvArrData.dRInterp = 0;
      %EnvArr = AcEnvArr(EnvArrData);
      %% Set up input file
      %WriteToRamFiles(EnvArr, RunDef, Freq, EnvFile);
      %fclose(EnvFile);
      %if RunDef.ManualEnvEdit
      %   lEditFile([FName '.env']);
      %end


   case {'Scooter', 'Scooter+Fields'}
      %Set up the required input files
      WriteToScooterFile(Env, RunDef, Freq, EnvFile);
      fclose(EnvFile);
      if RunDef.ManualEnvEdit
         lEditFile([FName '.env']);
      end
      switch PropgnCode
         case {'Scooter+Fields'}
            %Set up the required input files
            WriteToFieldsFiles(Env, FlpFile, RunDef.RMin, RunDef.RMax, RunDef.NRange)
            fclose(FlpFile);
            if RunDef.ManualEnvEdit
               lEditFile([FName '.flp']);
            end
      end


end % Switch Propgn

DirInfo = Env.DirInfo;
LogName = [FName '.prn'];

%******************************************************************************************************************
% experiment with a method possibly more globally useful if we're going to fully split Kraken*/Field Bounce/Bellhop

%
% this could be defined for RunDef setup (i.e. as intended for cell array RunID) - instead we create it here
% deliberately use RunIDs (plural) to avoid possible overwrite of RunID = RunDef.RunID if used somewhere in this code

RunIDs = {};
switch PropgnCode
   case 'Scooter+Fields'
      % order is important !!
      RunIDs = {'Scooter','Fields'};
   case 'Fields'
      % order is important !!
      RunIDs = {'Fields-Prep','Fields'};
   otherwise
      RunIDs = {PropgnCode};
end %switch PropgnCode

while ~isempty(RunIDs)   
   lExecCmd('Reset', EchoCommands);
   % RUN PROPAGATION CODES
   PCode       = RunIDs{1};
   RunIDs(1)   = []       ;
   switch PCode
      case {'Bounce', 'Bounce+Bellhop'},
         CmdString = ['"' DirInfo.Kraken 'bounce.exe" < "' FName '.env"  > "' LogName '"'];
         lExecCmd(CmdString);

         if UsePltrth
            %Convert this into a bellhop compatible file
            %plotrth.exe does most of this, but generates a whole lot of unwanted stuff at the
            %start of the file that has to be stripped out
            CmdString = ['"' DirInfo.Kraken 'plotrth.exe" >> "' FName '.tmp"'];
            lExecCmd(CmdString);

            lExecCmd('Execute');

            %Strip out the garbage
            lStripGarbage(FName);

            lExecCmd('Reset', EchoCommands);
            lExecCmd('del IRCFIL');
            lExecCmd('del plotrth.plp');
            CmdString = ['del "' FName '.tmp"'];
            lExecCmd(CmdString);
         else
            CmdString = ['copy BRCFIL "' FName '.brc" >> "' LogName '"'];
            lExecCmd(CmdString);

            lExecCmd('del IRCFIL');
         end

         switch PCode
            case 'Bounce'
               lExecCmd('del BRCFIL');
               lExecCmd('Execute');

            case 'Bounce+Bellhop',
               if UsePltrth
                  %Put the bottom reflection coefft file in the right place
                  CmdString = ['copy "' FName '.brc" BRCFIL  >> "' LogName '"'];
                  lExecCmd(CmdString);
               end

               if RunDef.UseBathFile
                  CmdString = ['copy "' RunDef.BathFName '" BTYFIL  >> "' LogName '"'];
                  lExecCmd(CmdString);
               end

               %Run Bellhop
               CmdString = ['"' DirInfo.Bellhop 'bellhop.exe" < "' FName '_2.env" >> "' LogName '"'];
               lExecCmd(CmdString);
               lExecCmd('del BRCFIL');
               if RunDef.UseBathFile
                  lExecCmd('del BTYFIL');
               end

               lExecCmd('Execute');

               FileInfo = dir('RAYFIL');  %Check if bellhop generated a ray file
               if ~isempty(FileInfo)
                  lExecCmd('Reset', EchoCommands);
                  CmdString = ['copy RAYFIL "' FName '.ray" >> "' LogName '"'];
                  lExecCmd(CmdString);
                  lExecCmd('del RAYFIL');
                  lExecCmd('Execute');
               end

               FileInfo = dir('ARRFIL');  %Check if bellhop generated an amplitude-delay file
               if ~isempty(FileInfo)
                  lExecCmd('Reset', EchoCommands);
                  CmdString = ['copy ARRFIL "' FName '.arr" >> "' LogName '"'];
                  lExecCmd(CmdString);
                  lExecCmd('del ARRFIL');
                  lExecCmd('Execute');
               end

         end

      case {'Kraken', 'KrakenC'},
         switch PCode
            case 'Kraken',
               if AppendFile
                  CmdString = ['"' DirInfo.Kraken 'kraken.exe" < "' FName '.env"  >> "' LogName '"'];
               else
                  CmdString = ['"' DirInfo.Kraken 'kraken.exe" < "' FName '.env"  > "' LogName '"'];
               end

            case 'KrakenC',
               if AppendFile
                  CmdString = ['"' DirInfo.Kraken 'krakenc.exe" < "' FName '.env"  >> "' LogName '"'];
               else
                  CmdString = ['"' DirInfo.Kraken 'krakenc.exe" < "' FName '.env"  > "' LogName '"'];
               end
         end
         lExecCmd(CmdString);

         %CmdString = ['copy MODFIL1 "' FName '.mod" >> "' LogName '"'];  %Do this to keep mode file
         CmdString = ['copy MODFil0001 "' FName '.mod" >> "' LogName '"'];  %Do this to keep mode file
         lExecCmd(CmdString);

         %Run Field to produce shade file
         CmdString = ['"' DirInfo.Kraken 'field.exe" < "' FName '.flp" >> "' LogName '"'];
         lExecCmd(CmdString);

         %lExecCmd('del MODFIL1');
         lExecCmd('del MODFil0001');
         lExecCmd('Execute');

      case {'RAM'}

         % THIS IS NOW OUTDATED .... NOT WORTH KEEPING UP TO DATE  ... RAM SHOULD ALWAYS BE RUN VIA AccEnvArr Class Methods

      case {'Scooter', 'Scooter+Fields'}
         %Run Scooter
         if AppendFile
            CmdString = ['"' DirInfo.Scooter 'scooter.exe" < "' FName '.env"  >> "' LogName '"'];
         else
            CmdString = ['"' DirInfo.Scooter 'scooter.exe" < "' FName '.env"  > "' LogName '"'];
         end
         lExecCmd(CmdString);
         CmdString = ['copy GRNFIL "' FName '.grn" >> "' LogName '"'];  %Do this to keep Greens function file
         lExecCmd(CmdString);
         % GRNFIL is not deleted in case there is a Scooter + Fields type call in which FIELDS is called immediately afterwards
         lExecCmd('Execute');

      case 'Fields-Prep',
         %Copy grn file to GRNFIL - this bit is inefficient if splitting Scooter+Fields into two loops (see exp code above switch)
         %Hence Fields-Prep is inserted only when Fields is called independently of Scooter
         copyfile([FName, '.grn'],'GRNFIL', 'f');

      case 'Fields',
         %Run Fields to produce shade file
         CmdString = ['"' DirInfo.Scooter 'fields.exe" < "' FName '.flp" >> "' LogName '"'];
         lExecCmd(CmdString);
         lExecCmd('del GRNFIL');
         lExecCmd('Execute');

   end % end switch PCode
end %switch while

% SAVE SHADE FILES
% ----------------
% NOTE: RAM* NOT COVERED HERE
%       - the RAM p.grid o/p file needs to be interpretted into shd format and naming is handled at that time
%         ... see the "RUN PROPAGATION CODES" case for RAM above
switch PropgnCode
   case {'Bounce+Bellhop', 'Fields', 'Kraken', 'KrakenC', 'Scooter+Fields'},  % NOT RAM*
      %Save shade file
      FileInfo = dir('SHDFIL');
      if ~isempty(FileInfo)   %Need this test as Bellhop doesn't always generate a shade file
         ShdName = [FName '.shd'];
         if AppendFile
            CmdString = ['copy "' ShdName '" + SHDFIL "' ShdName '" >> "' LogName '"'];
         else
            CmdString = ['copy SHDFIL "' ShdName '" >> "' LogName '"'];
            %if GName ~= 0, lAdd2ShdList(GName, ShdName); end
            if GName ~= 0,  AppendAsciiFileRecord([GName, '.shdl'], ShdName); end
         end
         lExecCmd('Reset', EchoCommands);
         lExecCmd(CmdString);

         lExecCmd('del SHDFIL');
         lExecCmd('Execute');
      end

end

ErrLevel = ScanPrnForErrors(LogName);
if ErrLevel == 0
   if EchoCommands
      disp('No errors or warnings found in log (.prn) file');
   end
else
   disp('ERROR and/or WARNING message(s) found in log (.prn) file');
end
disp(' ');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  lStripGarbage(FName)
InFile = fopen([FName '.tmp'], 'rt');
OutFile = fopen([FName '.brc'], 'wt');

Done = 0;

%Find the start of the data
while ~Done
   InStr = fgetl(InFile);
   if strcmp(InStr, 'TYPE xy');
      Done = 1;
   end
end

Count = 0;
Done = 0;
while ~Done
   InStr = fgetl(InFile);
   if strcmp(InStr, '&');
      Done = 1;
   else
      Count = Count+1;
   end
end

Done = 0;
while ~Done
   InStr = fgetl(InFile);
   if strcmp(InStr, 'TYPE xy');
      Done = 1;
   end
end

for ICount = 1:Count
   InStr = fgetl(InFile);
end

NPt = str2num(fgetl(InFile));
fprintf(OutFile, '%d\n', NPt);
for ICount = 1:NPt
   InStr = fgetl(InFile);
   fprintf(OutFile, '%s\n', InStr);
end

fclose(InFile);
fclose(OutFile);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lAdd2ShdList(FName, ShdName)
% Adds shade filename to the shade file list (.shdl file)

ListName = [FName '.shdl'];
fout = fopen(ListName, 'a');
if fout == -1
   % error ... should we write error note to LogName ?
else
   fprintf(fout, '%s\n', StripPath(ShdName, 'f') );
   fclose(fout);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lEditFile(FName)
eval(['!notepad "' FName '"']);
uiwait(msgbox('Edit file, save it, then OK to continue'));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lExecCmd(CmdStr, EchoCommands)
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
      if isempty(Fid) || Fid <= 0,
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