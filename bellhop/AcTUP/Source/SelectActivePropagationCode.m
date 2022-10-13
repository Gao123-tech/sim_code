function Def = SelectActivePropagationCode(Def)
%SelectActivePropagationCode       Exactly what is says ------------------------------------------------------
%
% USAGE:                  Def = SelectActivePropagationCode(Def);
%
% I/O                     DirInfo, Def --> See AcTUP
%                    
% Revision History
% ================
% Revision 0.0      11 July      2006 ... ALM (Viva l'Italia!!!)
%                   - local in ConfigEnvironmentAndPropagation by some other misspelt name 
%
% Revision 1.0      24 July      2006 ... ALM 
%                   - extracted
% -------------------------------------------------------------------------------------------------------------------------------
FALSE = 0;
TRUE  = 1;

ExitStrs = {'Help', 'CANCEL'};
done = FALSE;
while ~done
RunIDStr  = SelectPropagationCode([], ExitStrs, 'Select Active Model');
   switch RunIDStr
      case ExitStrs{1}
         PropagationCodeHelp;
      case ExitStrs{2}
         % nada
         done = TRUE;
      otherwise
         Def.RunID          = {RunIDStr};
         done               = TRUE      ;
   end
end