function  [Def, Status] = SetKrakenCParams(Def)
%SetKrakenCParams    Edits KrakenC-Specific Propagation parameters of Run Definition  ------------------
%
% USAGE:       [Def, Status] = SetKrakenCParams(Def);
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      xx ???????   ???? ... AJD
%                   - Originally incorporated in AcToolboxFrontEnd.m as lSetKrakencParams
% Revision 1.0      06 July      2006 ... ALM
%                   - Parsed into function
% --------------------------------------------------------------------------------------------------------

Ans = inputdlg({'Use exhaustive mode search (slow, but recommended for elastic problems - may give warnings) (y/n)'}, '', [1,100], {'y'});
if isempty(Ans)
   Status = 0;
   Def.Kraken.ExhaustiveSearch = 1;
else
   Status = 1;
   Def.Kraken.ExhaustiveSearch = strcmpi(Ans{1}, 'y');
end


