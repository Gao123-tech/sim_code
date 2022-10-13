function  DefOut = SetRAMParams(DefIn, DirInfo)
%SetRAMParams    Edits RAMx-Specific Propagation parameters of Run Definition  ------------------
%
% USAGE:       DefOut = SetRAMParams(DefIn, DirInfo);
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      xx ???????   ???? ... ALM
%                   -  Originally incorporated in AcToolboxFrontEnd.m as lSetRamParams
% Revision 0.1      14 October 2004   ... ALM
%                   -  Change line output default depth to 2nd shallowest depth if zmin == 0
%                      OR 1m if Zr = [0] only;
% Revision 0.2      25 February - 08 March 2005 ... ALM
%                   -  Alter interface so that user specifies dz as fraction of wavelength and dr as multiple of dz
%                   -  Extend this to a bunch of other parameters
% Revision 0.3      15 May 2005       ... ALM
%                   -  minor bug fixes
% Revision 1.0      06 July      2006 ... ALM
%                   - Parsed into function
% Revision 1.1      20 July      2006 ... ALM
%                   - Add minimum cs value to automatically accomodate fluid substrates (RAMS* seems to crash for cs=0)
% --------------------------------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


TRUE  = 1;
FALSE = 0;

% Initialise
done    = FALSE ;
Def     = DefIn ;
% GET USER INPUT for RAM specific data

% Establish which RAM to configure
while ~done
   Species = lSelectRAMSpecies();
   switch Species
      case 'Retain Edits & Exit'
         DefOut = Def;
         done   = TRUE; 
      case 'Discard Edits & Exit'
         butstr = questdlg('Confirm...', 'RAM* Parameters', 'DISCARD ALL EDITS','Back...','Back...');
         switch butstr
            case 'DISCARD ALL EDITS'
               DefOut = DefIn;
               done   = TRUE;
         end
      case 'Adapt Run Definition for RAM*'
         Def    = lAdaptRunDef4RAM(Def);
      otherwise
         Def    = lSetRAMParameters(Def, Species);
   end
end


% LOCALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function DefOut = lAdaptRunDef4RAM(Def)
% This takes care of cases where the Environment or RunDefinition model is incompatible with RAM

%initialisations
FALSE   = 0;
TRUE    = 1;
DefOut  = Def;
% each of the follwing sections are run such that most cancel 
% operations either give the user another go or invoke 'return'

% Def is only copied to DefOut if the code survives to the end 

% LINE OUTPUT - Rx DEPTH -------------------------------------------
% RAM can only do one receiver depth for tl.line output
% test
zrzero = FALSE;
if Def.Zr(1) == 0,
   zrzero = TRUE;
   zridx  = 2   ;
else
   zridx  = 1   ;
end
if length(Def.Zr) == 1 && ~zrzero
   Def.RAM.zr = Def.Zr;
else
   message    = {'RAM* can only process one receiver depth for .line output', ...
                 'This does not affect grid specification (.grid file)'};
   title      = 'RAM* Rx Configuration - more than receiver depth specified';
   buttstr1   = 'Specify new depth';
   buttstr3   = 'CANCEL';
   
   % loop question until ok or cancelled
   done = FALSE
   while ~done
      if length(Def.Zr) == 1
         buttonname = questdlg(message, title, buttstr1,         buttstr3, buttstr1);
      else
         buttstr2   = ['Use first non-zero depth (', num2str(Def.Zr(zridx)), 'm)'];
         buttonname = questdlg(message, title, buttstr1,buttstr2,buttstr3, buttstr1);
      end
      switch buttonname
         case buttstr1
            Ans = inputdlg({'receiver depth for transmission loss output (tl.line)'} ,'RAM Specific Input', [1,100], {num2str(Def.RAM.zr)});
            if isempty(Ans)
               % nothing
            else
               Def.RAM.zr = str2num(Ans{1});
               done       = TRUE;
            end
         case buttstr2
            Def.RAM.zr    = Def.Zr(zridx);
            done       = TRUE;   
         case buttstr3
            % CANCEL !
            return;
      end
   end
end

% END Rx DEPTH FOR LINE OUTPUT -------------------------------------


% RAM CANNOT DEAL WITH HALF-SPACES ---------------------------------
% has a bottom halfspace been defined
NEnv      = GetNumberOfElements(Def.EnvArr);
HalfSpace = FALSE;
%
for IEnv = 1:NEnv
   Env       = GetElement(Def.EnvArr, IEnv);
   LastLayer = GetLayer(Env, 'last');
   if IsHalfSpace(LastLayer)
      HalfSpace = TRUE;
      break; % for loop
   end
end
%LastLayer = GetLayer(Def.Environment, 'last');
%if IsHalfSpace(LastLayer)
if HalfSpace
   message    = 'RAM* cannot model a bottom layer halfspace';
   title      = 'RAM* Environment Model - bottom layer halfspace specified';
   buttstr1   = 'Specify maximum depth';
   buttstr2   = 'CANCEL';
   done       = FALSE;
   while ~done
      buttonname = questdlg(message, title, buttstr1,buttstr2,buttstr1);
      switch buttonname
         case buttstr1
            Ans = inputdlg({'thickness of layer to replace halfspace [*lambda]'}, ...
               'RAM* Environment Model', ...
               1, ...
               {num2str(Def.RAM.LastLayerDz_lambda)});
            if ~isempty(Ans),
               Def.RAM.LastLayerDz_lambda = str2num(Ans{1});
               done = TRUE;
            end
         case buttstr2
            %Cancel 
            return;
      end % switch
   end % while
else
   % NO LONGER NEED TO SPECIFY THIS HERE - f-DEPENDENT !
   % even if it has finite thickness, store the thickness in Def.RAM so
   % only have to look in one object irrespective of case
   % Def.RAM.LastLayerDz = GetLayerThickness(LastLayer);
end

% END HALFSPACE REPLACEMENT    -------------------------------------

DefOut = Def;


% LOCALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function DefOut = lSetRAMParameters(Def, Species);
% This takes care of cases where the Environment or RunDefinition model is incompatible with RAM

%initialisations
FALSE   = 0;
TRUE    = 1;
DefOut  = Def;

% Get (easy) RAM specific data --------------------------------------------------------------------
% Basic Prompts
Prompt{ 1} = 'Maximum depth for TL grid output [m]   {-1 = use max environment depth}';
Prompt{ 2} = 'Relative depth resolution [*lambda]    {Jensen et al. ~ < 1/4  - MAX #points is 8000}';
%Prompt{ 3} = 'Grid OP depth decimation factor        {integer >= 1}';
Prompt{ 3} = 'Minimum OP grid depth resolution    [m]';
Prompt{ 4} = 'Relative range resolution [*dz]        {Jensen et al. ~ 2(0)-5(0) with(out) bottom interaction}';
%Prompt{ 5} = 'Grid OP range decimation factor        {integer >= 1}';
Prompt{ 5} = 'Minimum OP grid range resolution    [m]';
Prompt{ 6} = 'Reference phase velocity [m/s]';
Prompt{ 7} = 'Number of terms in Padé expansion      {MAX 10}';
Prompt{ 8} = 'Number of stability constraints';
Prompt{ 9} = 'Max range of stability constraints [m] {0 = full range}';
Prompt{10} = 'Attenuation layer thickness [*lambda]';
Prompt{11} = 'Attenuation layer maximum p-wave attenuation [db/lambda]';
Prompt{12} = 'Attenuation layer maximum s-wave attenuation [db/lambda]';
Prompt{13} = 'Minimum shear velocity in substrate [m/s]';
% Default Values
DefAns{ 1} = num2str(Def.RAM.zmplt)       ;
DefAns{ 2} = num2str(Def.RAM.dz_lambda)   ;
%DefAns{ 3} = num2str(Def.RAM.ndz)         ;
DefAns{ 3} = num2str(Def.RAM.dzgridmin)   ;
DefAns{ 4} = num2str(Def.RAM.dr_dz)       ;
%DefAns{ 5} = num2str(Def.RAM.ndr)         ;
DefAns{ 5} = num2str(Def.RAM.drgridmin)   ;
DefAns{ 6} = num2str(Def.RAM.c0)          ;
DefAns{ 7} = num2str(Def.RAM.np)          ;
DefAns{ 8} = num2str(Def.RAM.ns)          ;
DefAns{ 9} = num2str(Def.RAM.rs)          ;
DefAns{10} = num2str(Def.RAM.AttenLayerDz_lambda);
DefAns{11} = num2str(Def.RAM.AttenLayerAttenPMax);
DefAns{12} = num2str(Def.RAM.AttenLayerAttenSMax);
DefAns{13} = num2str(Def.RAM.CsMin)       ;

% Delete or Switch Non-Applicable Parameters
switch Species
   case {'RAM', 'RAMGeo', 'RAMSurf'}
      Prompt(12:13) = [];
      DefAns(12:13) = [];
   case {'RAMS', 'RAMSGeo'}
      % switch
      Prompt{ 8} = 'irot';
      DefAns{ 8} = num2str(Def.RAM.irot);
      Prompt{ 9} = 'theta';
      DefAns{ 9} = num2str(Def.RAM.theta);
end

% GET INPUT ---------------------------------------------------------------
cancel = FALSE;
Ans = inputdlg(Prompt,'RAM specific parameters', [1,100], DefAns);

if isempty(Ans)                     % CANCEL
   cancel   = TRUE;
else                                % GOT DATA
   % Common to RAM*
   Def.RAM.zmplt               = str2num(Ans{ 1});
   Def.RAM.dz_lambda           = str2num(Ans{ 2});
   %Def.RAM.ndz                 = str2num(Ans{ 3});
   Def.RAM.dzgridmin           = str2num(Ans{ 3});
   Def.RAM.dr_dz               = str2num(Ans{ 4});
   %Def.RAM.ndr                 = str2num(Ans{ 5});
   Def.RAM.drgridmin           = str2num(Ans{ 5});
   Def.RAM.c0                  = str2num(Ans{ 6});
   Def.RAM.np                  = str2num(Ans{ 7});
   Def.RAM.AttenLayerDz_lambda = str2num(Ans{10});
   Def.RAM.AttenLayerAttenPMax = str2num(Ans{11});

   % Species Specific
   switch Species
      case {'RAM', 'RAMGeo','RAMSurf'}
         Def.RAM.ns                  = str2num(Ans{ 8});
         Def.RAM.rs                  = str2num(Ans{ 9});
      case {'RAMS', 'RAMSGeo'}
         % switch
         Def.RAM.irot                = str2num(Ans{8});
         Def.RAM.theta               = str2num(Ans{9});
         Def.RAM.AttenLayerAttenSMax = str2num(Ans{12});
   end
end

% one more input
if ~cancel 
   switch Species
      case 'RAMSurf'
         Def = GetSurfaceFilename(Def, DirInfo);
   end
end

% return modified Def if OK
if ~cancel 
   DefOut = Def;
end

% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lDisplayHelp;
hmsg = {'Each RAM species selected will require slightly different parameter set', ...
        '' ...
        ['In this version, each time you select a code variant you are shown all ', ...
         'parameters relevant for that variant, even though they may be shared ', ...
         'by other variants'],...
        '', ...
        ['By changing a shared parameter you are changing that parameter for all ', ...
        'runs using RAM variants'], ...
        '', ...
        'The only non-universal parameters are:',...
        '   free surface profile     ... RAMSurf',...
        '   irot, theta                   ... RAMS, RAMSGeo', ...
        '   ns, rs                         ... RAM , RAMGeo , RAMSurf', ...        
        '         where',...
        '                 ns  = number of stability constraints', ...
        '                 rs   = max range of stability constraints', ...
        '', ...        
        '', ...
        'There is a set of base parameters that require greater user interaction...', ...
        '', ...
        ['The option  << Adapt Run Definition for RAM* >>  allows you to do this ', ... 
        'once for all RAM variants'], ...
        '', ...
        ['This should be done whenever the environment or model-independent ', ...
        'propagation parameters are editted'], ...
        '', ...
        ['There are currently no checks so the user needs to be aware to invoke this ', ...
        'option as required'], ...
       };
uiwait(msgbox(hmsg,'Specifying Propagation Parameters for RAM variants','help','modal'));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Species = lSelectRAMSpecies
% species = 'CANCEL' if user selects cancel 
FALSE = 0;
TRUE  = 1;
options = [RAMSpecies(), {'Adapt Run Definition for RAM*', 'Help', 'Retain Edits & Exit', 'Discard Edits & Exit'}];
done    = FALSE;
Species = [];
while ~done   
   idx = menu('Select RAM type', options);
   switch options{idx}
      case 'Help'
         lDisplayHelp;
      otherwise
         Species = options{idx};
         done    = TRUE;
   end
end
