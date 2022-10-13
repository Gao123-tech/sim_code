function DefOut = EditRunDefModelDependent(DefIn, DirInfo)
%EditRunDefModelDependent    Edits Propagation Model-Dependent parameters of Run Definition  -----------------------
%
% USAGE:       Def = EditRunDefModelDependent(Def);
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      xx ???????   ????
%                   - Originally incorporated in AcToolboxFrontEnd.m  (various subs like lSet***Params : *** = RAM, Kraken, Bellhop, etc ...)
% Revision 1.0      10 July      2006 ... ALM   Viva l'Italia!!
%                   - Parsed into function
% -----------------------------------------------------------------------------------------------------------

TRUE  = 1;
FALSE = 0;

ExitStrs = {'RAM*', 'Help', 'Exit Saving Changes', 'Exit & Don''t Save Changes'};
HelpStr  = {'Not all propagation codes require additional information over the Code-Independent set of data.', '', ...
            'For example a Scooter+Fields run is completely specified by the common parameter set.'};

% copy input Definition
Def    = DefIn     ;
done   = FALSE     ;
while ~done
   %Select Model to Edit
   CodeID = SelectPropagationCode([2 3 5], ExitStrs, 'Edit Parameters for:');
   %Edit
   switch CodeID
      case 'Bounce+Bellhop',
         Def = SetBellhopParams(Def);
      case 'Fields',
         Def = SetFieldsParams(Def);
      case 'KrakenC',
         Def = SetKrakenCParams(Def);
      case ExitStrs{1},  % need to replace the two RAM versions with a single option
         Def = SetRAMParams(Def, DirInfo);
      case ExitStrs{2}         
         hdl = helpdlg(HelpStr, 'Where are the other propagation codes ?');
         set(hdl, 'WindowStyle', 'modal');
         waitfor(hdl);
      case ExitStrs{3}         % no needed yet (doesn't do much but might do later)
         Def    = UpdateRunDefinition(Def);
         DefOut = Def;         
         done   = TRUE;
      case ExitStrs{4}         
         butstr = questdlg('Confirm...', 'Model Dependent Propagation Specification', 'DISCARD ALL EDITS','Back...','Back...');
         switch butstr
            case 'DISCARD ALL EDITS'
               DefOut = DefIn;
               done   = TRUE;
         end
   end
end

