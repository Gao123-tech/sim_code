function [Def, DirInfo, ButtonStr] = ConfigEnvironmentAndPropagation(Def, DirInfo, ExitFlags, ExitStrs) 
%ConfigEnvironmentAndPropagation       What it is ...  for AcTUP ------------------------------------------------------
%
% USAGE:                  [DirInfo, Def, ButtonStr] = ConfigEnvironmentAndPropagation(DirInfo, Def, ExitFlags, ExitStrs);
%
% INPUT        DirInfo, Def --> See AcTUP
%
%              ExitFlags      = This is a vector of 7 boolean flags - one for each of the selectable operations excluding 'EXIT'
%
%                                'Load DEFAULT Run Definition'
%                                'Load Run Definition'
%                                'Save Run Definition'
%                                'Edit Environment'                          
%                                'Edit Common Propagation Parameters'        
%                                'Edit Model-Specific Propagation Parameters'
%                                'Select Active Model ***'                 
%
%                               Note 'EXIT' is not selectable because ExitStr is always returned (since it end the call anyway)
%
%                               When these flags are set TRUE, thea ssociated operations are not invoked 
%                               The function exits (see Output) returning control to caller
%
%                               These are a list of functions that are normally available to CE&P as a standalone function
%                               but to which the CE&P's calling program may require restricted access or additional return control
%                               (i.e. other than via CE&P)
%
%                               [] or omit for autonomous operation
%
%              ExitStrs       = Cell array of strings which will appears as additional buttons and behave as as EXIT command
%                               If omitted or empty, the EXIT button string will be 'EXIT. These can be used to expand choices
%                               selected via CE&P but dealt with by calling program.
%
% OUTPUT       ButtonStr      = Button string that caused exit (see SetExitVec)
%                        
% Revision History
% ================
% Revision 0.0      11 July      2006 ... ALM (Viva l'Italia!!!)
%                   - extract and reorganised from main menu opteration in AcTUP
%                   - still uses combined Env and Run Def ... AG's modular interface much nicer - maybe future ver or integration
%
% Revision 0.01     24 July      2006 ... ALM (Cham-one MFs)
%                   - change Default RunDef to from file mode
%                   - Some changing of words (menu: model <- code)
%                   - Extract some locals to functions 
% -------------------------------------------------------------------------------------------------------------------------------

% ALM's std initialisation
TRUE  = 1;
FALSE = 0;
% Menu Definitions and Enumerations
iModInd  = 5;
iSAMOpt  = 7;
SAMOpt   = BuildSAMOpt(Def.RunID);
BaseStrs = ...
   {'Load DEFAULT Run Definition'                    , ...       % case  1
    'Load Run Definition'                            , ...       % case  2
    'Save Run Definition'                            , ...       % case  3
    'Edit Environment'                               , ...       % case  4
    'Edit Code-Independent Propagation Parameters'   , ...       % case  5
    'Edit Code-Dependent Propagation Parameters'     , ...       % case  6 
     SAMOpt                                            };        % case  7

 % above works because run ID at the ost has only 1 element in this version 
 % - won't work for any future multiple active models but the interface will change anyway!
 
%auto 
NBaseStrs = length(BaseStrs);

% INPUT check 
% -> exit flags can either be omitted, empty or the correct length
if ~exist('ExitFlags', 'var'), ExitFlags = []; end
if isempty(ExitFlags)
   ExitFlags = zeros(1, NBaseStrs);
elseif length(ExitFlags) ~= NBaseStrs
   error({'ERROR: ConfigEnvironmentAndPropagation -> ExitFlags parameter is incorrect length', 'must be same length possible outputs'});
end
% -> make sure there is at least one ExitStr
if ~exist('ExitStrs', 'var'), ExitStrs = []; end
if isempty(ExitStrs)
   ExitStrs = {'EXIT'};
end
%-> check DirInfo
if ~exist('DirInfo', 'var'), DirInfo = []; end
if isempty(DirInfo)
   DirInfo = GetAcDirectoryInfo;
end
% -> check Def
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
         [StateIdx, LaFine] = lMainMenu(Def, DirInfo, MenuStrs, ExitFlags, iModInd);
      % =============================================================================================
      otherwise
         State = MenuStrs{StateIdx};
         switch State
            case 'Load DEFAULT Run Definition' 
               Def                = LoadRunDefinition(DirInfo, -1)  ;
               DirInfo.LastRunDef = DirInfo.RunDef                  ;
               Def                = UpdateRunDefinition(Def)        ;
               MenuStrs{iSAMOpt}  = BuildSAMOpt(Def.RunID)         ;               
            case 'Load Run Definition' 
               [Def, DirInfo]     = lLoadRunDef(Def, DirInfo)       ;
               Def                = UpdateRunDefinition(Def)        ;               
               MenuStrs{iSAMOpt}  = BuildSAMOpt(Def.RunID)         ;               
            case 'Save Run Definition'
               % Update RunDefinition before save
               Def                = UpdateRunDefinition(Def)        ;
               [Def, DirInfo]     = SaveRunDef(Def, DirInfo)        ;
            case 'Edit Environment'
               Def.EnvArr         = EditGUI(Def.EnvArr)             ;
               Def.Environment    = GetElement(Def.EnvArr, 1)       ;  %First element of environment array used for range independent codes
            case 'Edit Code-Independent Propagation Parameters'
               Def                = EditRunDefModelIndependent(Def, DirInfo)  ;
            case 'Edit Code-Dependent Propagation Parameters'
               Def                = EditRunDefModelDependent(Def, DirInfo)    ;
            case MenuStrs{iSAMOpt}
               Def                = SelectActivePropagationCode(Def);
               MenuStrs{iSAMOpt}  = BuildSAMOpt(Def.RunID)         ;
         end         
         StateIdx = MainMenuStateIdx; % this could vary for each Input State but not in this case
         % =============================================================================================
   end
end
%Return Last Button Press (can only be an Exit mode and can never be MainMenu)
ButtonStr = MenuStrs{StateIdx}       ;
% Update RunDefinition before return
Def       = UpdateRunDefinition(Def) ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%LOCAL%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [DefOut, DirInfo] = lLoadRunDef(DefIn, DirInfo)
% this bit is based on lOpenRunDef from acToolboxFrontEnd
DefOut   = DefIn;
Here    = pwd   ;
Status  = 1     ;
FileOK  = 1     ;
if exist(DirInfo.LastRunDef, 'dir')
   % use this to look for RunDef first
elseif ~exist(DirInfo.RunDef, 'dir')
   Status = LibMakeDirectory(DirInfo.RunDef);
   uiwait(warndlg(['Couldn''t make directory ' DirInfo.RunDef]));
   return;  
end

cd(DirInfo.LastRunDef);
[File, Path] = uigetfile('*.mat', 'Run definition file');
cd(Here);
if File == 0  % Cancelled
   DefOut = DefIn;
   return;
end
%load([Path File]);   %loads Def structure
% replace with new function for handling global run def
[Def, FileOK]      = LoadRunDefinition(DirInfo, [Path File]);
DirInfo.LastRunDef = Path;
if ~FileOK
   errordlg({'Error loading specified run definition file', [Path File], 'Last definition returned'},'ConfigEnvironmentAndPropagation','modal');
   return;
end
%based on AcToolBoxFrontEnd.m: case 'OpenRunDef'
if isempty(Def)
   return;
end
if ~isfield(Def, 'SubDir')
   Def.SubDir = '';
end
%Stuff for backward compatibility -----------
if ~isfield(Def, 'FPrefix')
   Def.FPrefix = 'Test';
end
if ~isfield(Def, 'ManualEnvEdit')
   Def.ManualEnvEdit = 0;
end
if ~isfield(Def, 'EnvArr')
   EnvArrInfo.Name = 'From old run def environment';
   EnvArrInfo.EnvArr{1} = Def.Environment;
   EnvArrInfo.RangeVec = 0;
   EnvArrInfo.dRInterp = 0;
   Def.EnvArr = AcEnvArr(EnvArrInfo, DirInfo);
end
% --------------------------------------------
DefOut = Def;


%LOCAL%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [StateIdx, LaFine] = lMainMenu(Def, DirInfo, MenuStrs, ExitFlags, iModInd)
FALSE    = 0;
TRUE     = 1;
LaFine   = FALSE;
StateIdx = menu( 'Environment & Propagation Definitions', MenuStrs );
if ExitFlags(StateIdx); 
   LaFine = TRUE;
   % Build and Test output paths and filename (base) - must be done here and not in Edit or Load since each one
   % can bypass the other
   GroupName = BuildAndTestOutputDir(Def, DirInfo, TRUE);
   if isempty(GroupName) % there is one of two problems both fixable via EditModelIndependent
      butstr = questdlg(['Do you want to ', MenuStrs(iModInd), ' now?'], 'Target Folder/Filename Error', 'EDIT','Ignore','EDIT');
      switch butstr
         case 'EDIT'
            % override exit and point to approriate editor
            StateIdx = iModInd ;
            LaFine   = FALSE   ;
      end
   end
end
      
