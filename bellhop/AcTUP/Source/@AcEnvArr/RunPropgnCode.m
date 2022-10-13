function [Status, ErrStr] = RunPropgnCode(EnvArrObj, RunDef, Freq, GName, FName, AppendFile, PropgnCode, EchoCommands)
% RunPropgnCode  Creates the necessary input files to run, and runs, the specified acousitc propagation code 
%                For available functions see PropgnCode argument description below
%
% USAGE:     [Status, ErrStr] = RunPropgnCode(EnvArrObj, RunDef, Freq, GName, FName, AppendFile, PropgnCode, EchoCommands);
%
% Freq       - frequency (Hz)
% GName      - group filename 
%              -> (relative) path + prefix only (no extension)
%              -> routine will append the filname (sans path) of the .shd
%                 file (if generated) to this file (GName.shdl)
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
% PropgnCode - determines acoustic propagation code to be run:
%              -> 'Bellhop'    Gaussian bean tracing program - requires additional information in ExtraParams structure
%              -> 'Bounce'     Seabed reflection coefficient calculation
%              -> 'Kraken'     Normal mode code
%              -> 'KrakenC'    Complex arithmetic normal mode code
%              -> 'RAM'        Parabolic equation (PE) code 
%                              (RAMGeo only at this stage)
%              -> 'Scooter'    Fast-field program
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
%
% Revision 0.1  26 May      2003   ALM
%               - Add RAM(Geo) 
%
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
% Revision 1.0  02-13 December 2004   ALM 
%               - modified to use expanded and consolidated Run Definition structure (not yet a class)
%               - reduced parameter set is incompatible with previous versions
%               - general append function used to write shdl file
%
% Revision 1.1  07 March    2005   ALM 
%               - Modification to call to WriteShadeFile to cope with apparent RAMGeo behaviour
%                 * for some reason it looks like RAMGeo outputs grid from z=0 rather than z=ndz*dz 
%                   which wasn't expected from looking at the RAMGeo FORTRAN source code 
%                 * comparisons with Scooter and Kraken indicate that the RAMGeo o/p shade file grid is always
%                   shifted ndz*dz (deeper) ... this has been tested with ndz == 1 and ndz > 1
%                 * I'M STILL CONCERNED SOMETHING ELSE IS GOING ON BUT THIS SEEMS TO WORK ! (DODGEY?)
% Revision 1.2  09 March    2005   ALM 
%               - Apply a pi/2 phase correction to PGrid data 
%                  * Results appear to agree with Scooter and KrakenC (and Bellhop) resutls for at least one case so far
%                  * Suggests RAMGeo and RAMSGeo could be modified to o/p more sensible PGrid 
%                    -> looks like the pi/4 correction i applied in FORTRAN (after Jensen et al., COA) is in the wrong direction
%                    -> of course it would be because i didn't take into account at that time that the PGrid o/p also requires 
%                       conjugation for it to agree with Scooter et al. ... so yeah the initial pi/4 correction is in the wrong direction
% Revision 1.3  16 March    2005   ALM 
%               - Apply correction to PGrid for RAMSGeo - this is different to the RAMGeo o/p 
%                  *** in the near future RAM*Geo SOURCE CODE needs to be modified so that we have consistent .pgrid files
%
% Revision 1.4  21 March    2005   ALM 
%               - Apply correct definition of rd vectors for RAMGeo and RAMSGeo following some simple trials immediately preceding
%                 this date involving varying ndz.
%
% Revision 1.5  01 April    2005   ALM 
%               - temp mod to find RAMSGeo phase correction
%
% Revision 1.6  04 April    2005   ALM 
%               - Move phase corrections to FORTRAN code 
%                                                         RAMGeo  version 1.5C00.03
%                                                         RAMSGeo version 0.5C01.01
%               - version 1.3 & 1.5 mods have been deleted to clean up code ... see archived v1.5 for code details
%
% Revision 1.7  20 April    2006   ALM 
%               - Add species tag to title -> shade file
%
% Revision 2.0  12 July     2006   ALM 
%               - Modified to use increased resolution in Propagation Code spec
%
% Revision 2.01 21 July     2006   ALM 
%               - Minor bug fix (Def.RunID -> Def.RunID{1})
%               - Copes with changes to RunIDs (Bellhop changed to Bounce+Bellhop.. although Bellhop retained for future solo use)
%

NARGINMAX = 10;
if nargin < NARGINMAX
    EchoCommands = 1;
end


switch PropgnCode
    case {'Bellhop', 'Bounce', 'Bounce+Bellhop', 'Fields', 'Kraken', 'KrakenC', 'Scooter', 'Scooter+Fields'} 
        %Range independent codes use the first element of the environment array
        EnvObj = EnvArrObj.EnvArr{1};
        [Status, ErrStr] = RunPropgnCode(EnvObj, RunDef, Freq, GName, FName, AppendFile, PropgnCode, EchoCommands);
        
    case {'RAMGeo', 'RAMSGeo'}
        [Status, ErrStr] = lRunRAM(EnvArrObj, RunDef, Freq, GName, FName, AppendFile, PropgnCode, EchoCommands);
                
    otherwise
        uiwait(warndlg(['RunPropgnCode - Unknown propagation code requested: ' PropgnCode]));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [Status, ErrStr] = lRunRAM(EnvArrObj, RunDef, Freq, GName, FName, AppendFile, PropgnCode, EchoCommands);

Status = 1;

% open environment file
ErrStr = 'OK';
EnvFile = fopen([FName '.env'], 'wt');
if EnvFile <= 0
    Status = 0;
    ErrStr = ['couldn''t open environment file: '  FName '.env'];
end


if Status
    lExecCmd('Reset', EchoCommands);
    
    % WRITE SETUP FILES FOR PROPAGATION CODES 
    
    % Set up input file
    WriteToRamFiles(EnvArrObj, RunDef, Freq, EnvFile);
    fclose(EnvFile);
    if RunDef.ManualEnvEdit
        lEditFile([FName '.env']);
    end
    
    DirInfo = GetDirInfo(EnvArrObj);
    
    LogName = [FName '.prn'];
    
    % WRITE A COPY OF THE RUN DEF MAT FILE
    SaveRunDefinition(RunDef, [FName, '.def']);
    
    % RUN PROPAGATION CODES

    % Copy env file to standard input file name (ram[whatever].in) force
    % overwrite (Y switch)
    % dos messages to log file    

    %MOD v2.0 - The Species tag is not used for this version - instead the user selects directly selects which code is active
    % any commonality is taken care of by grouping responses to both {'RAMGeo' & 'RAMSGeo'} for swtiches previously looking for 'RAM'
    %RAMVersionData = GetRAMVersionData(RunDef.RAM.Species);
    RAMVersionData = GetRAMVersionData(RunDef.RunID{1});    
    ExeName        = RAMVersionData.ExeName;
    InName         = RAMVersionData.InName;
    PGridName      = RAMVersionData.PGridName;
    PLineName      = RAMVersionData.PLineName;
    TLGridName     = RAMVersionData.TLGridName;
    TLLineName     = RAMVersionData.TLLineName;

    CmdString = ['copy /Y "' FName '.env" "' InName '" > "' LogName '"'];    
    lExecCmd(CmdString);
    % run RAM, with explicit path in call, from current directory and
    % append any dos messages to log file
    CmdString = ['"' [DirInfo.Ram, ExeName] ' " >> "' LogName '"'];
    lExecCmd(CmdString);
    % delete input file 
    CmdString = ['del "' InName '" >> "' LogName '"'];
    lExecCmd(CmdString);
    % copy standard output files tl.line and tl.grid to storage files
    % copy nonstandard complex pressure files p.line and p.grid to storage files
    % DOS command "move" can rename file at as well as move it if only one file is specified at a time
    % switch /Y suppresses overwrite confirmation 
    CmdString = ['move /Y ' TLGridName ' "' FName '.grid"  >> "' LogName '"'];  %  std  RAM* transmission loss files
    lExecCmd(CmdString);
    CmdString = ['move /Y ' TLLineName ' "' FName '.line"  >> "' LogName '"'];  %  std  RAM* transmission loss files
    lExecCmd(CmdString);
    CmdString = ['move /Y ' PGridName  ' "' FName '.pgrid" >> "' LogName '"'];  %  CMST RAM* pressure (complex) files
    lExecCmd(CmdString);
    CmdString = ['move /Y ' PLineName  ' "' FName '.pline" >> "' LogName '"'];  %  CMST RAM* pressure (complex) files
    lExecCmd(CmdString);
    lExecCmd('Execute');
    % create grid pressure file in MP's shade file format
    %RamIn = ReadRamInFile(RunDef.RAM.Species, [FName, '.env']);
    RamIn = ReadRamInFile(RunDef.RunID{1}, [FName, '.env']);
    % change v0.4 --------------
    %PGrid = ReadRamPGrid(RunDef.RAM.Species , [], [FName, '.pgrid']); % [] use current/version parameters
    PGrid = ReadRamPGrid(RunDef.RunID{1}, [], [FName, '.pgrid']); % [] use current/version parameters
    % --------------------------
    if ~isempty(PGrid)
       nzplt = size(PGrid,1);
       nrplt = size(PGrid,2);
       dzplt = RamIn.dz*RamIn.ndz;
       drplt = RamIn.dr*RamIn.ndr;
       rr    = (1:1:nrplt) .* drplt;
       % Revision 1.4 - see note in header
       rd    = (1:1:nzplt) .* dzplt;
       % end Revision
       switch RunDef.RunID{1}
          case 'RAMGeo'
             % Revision 1.4 - see note in header
             % pull rd back one place - same spacing as RAMGeo yields identically zero TL for first depth when ndz==1 (ie freesurface)             
             rd    = rd-RamIn.dz;
             % end Revision
          case 'RAMSGeo'
             % OK
       end
       
       WriteShadeFile([RunDef.RunID{1}, ' - ', RamIn.title], ...
                      RamIn.freq, ...
                      RamIn.zs, ...
                      rd, ...
                      rr, ...
                      PGrid, ...                                           
                      [FName, '.shd']);
       if GName ~= 0,  AppendAsciiFileRecord([GName, '.shdl'], [FName, '.shd']); end
    
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
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function  lStripGarbage(FName);
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
function lAdd2ShdList(FName, ShdName);
% Adds shade filename to the shade file list (.shdl file)

ListName = [FName '.shdl'];
fout = fopen(ListName, 'a');
if fout == -1 then
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
      if isempty(Fid) | Fid <= 0, 
         Fid = fopen('CommandStr.bat', 'wt');
         if ~EchoCommands, fprintf(Fid, 'echo off\n'); end
      else
         disp('WARNING->RunPropgnCode: attempt to open new dos batch file before closing existing'); 
      end
      
   case 'Execute',     % CLOSE and EXECUTE batch file
      fclose(Fid);
      Fid = 0;
      clear('Fid');
      !CommandStr.bat
      
   otherwise,          % WRITE Command string to batch file
      fprintf(Fid, '%s\n', CmdStr);
end