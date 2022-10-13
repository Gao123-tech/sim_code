function Def = EditRunDefModelIndependent(Def, DirInfo)
%EditRunDefModelIndependent    Edits Model independent paramters of Run Definition (Propagation Spec) ----------------
%
% USAGE:       Def = EditRunDefModelIndependent(Def);
%
% See AcTUP for object definitions
% Use SaveRunDefinition if not using DirInfo and related support not required
% Revision History
% ================
% Revision 0.0      xx ???????   ????
%                   - Originally incorporated in AcToolboxFrontEnd.m  (case 'EditRunDef')
% Revision 1.0      06 July      2006 ... ALM
%                   - Parsed into function
%                   - removed save operationat the end
%                   - add filename spec
%                   - add or reintroduce Bathymerty filename switch and filename spec
% -----------------------------------------------------------------------------------------------------------

%Revision AcTUP_v2.01 - IP previously included "# range steps"
%                       Actually this was incorrect (at least for RAM)
%                       Changed to "range slices" which is one more than the resultant # range steps

NoYes  = {'n','y'};

%Mod the environment parameters
Prompt = {  'Title'                                      , ...
            'Frequency(s) [Hz]'                          , ...
            'Source depth [m]'                           , ...
            'Receiver depth(s) [m]'                      , ...
            'Minimum range [m]'                          , ...
            'Maximum range [m]'                          , ...
            'Number of range slices (#steps + 1)'        , ...
            'Sub directory for output files'             , ...
            'Filename prefix for output files'           , ...
            'Use bathymetry file where supported {y/n}'  , ...
            'Allow manual edit of environment file {y/n}' };

DefAns = {  Def.Title                                    , ...
            num2str(Def.Freq)                            , ...
            num2str(Def.Zs)                              , ...
            num2str(Def.Zr)                              , ...
            num2str(Def.RMin)                            , ...
            num2str(Def.RMax)                            , ...
            num2str(Def.NRange)                          , ...
            Def.SubDir                                   , ...            
            Def.FPrefix                                  , ...
            NoYes{Def.UseBathFile+1}                     , ...
            NoYes{Def.ManualEnvEdit+1}                    };

RetAns = inputdlg(Prompt,'Propagation model parameters', [1,80], DefAns);

if ~isempty(RetAns)
   Def.Title   = RetAns{1};
   Def.Freq    = str2num(RetAns{2});
   Def.Zs      = str2num(RetAns{3});
   Def.Zr      = str2num(RetAns{4});
   Def.RMin    = str2num(RetAns{5});
   Def.RMax    = str2num(RetAns{6});
   Def.NRange  = str2num(RetAns{7});
   Def.SubDir  = RetAns{8};
   if ~isempty(Def.SubDir)
      if Def.SubDir(end) ~= '\'
         Def.SubDir = [Def.SubDir '\'];
      end
   end
   Def.FPrefix       = RetAns{9}               ;   
   Def.UseBathFile   = strcmpi(RetAns{10}, 'y');
   Def.ManualEnvEdit = strcmpi(RetAns{11}, 'y');

   % get filename if reqd (second chance to refuse also)
   if Def.UseBathFile
      Def = GetBathymetryFilename(Def, DirInfo);
   end
   
   %update
   Def = UpdateRunDefinition(Def);  
   %save(DefFName, 'Def');   %Save the new settings   
end