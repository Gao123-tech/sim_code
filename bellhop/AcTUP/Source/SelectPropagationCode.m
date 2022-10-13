function CodeID = SelectPropagationCode(ActiveIdx, ExitStrs, Prompt)
%SelectPropagationCode    Selects Propagation Code (ID) -----------------------------------------------------
%
% USAGE:       CodeID        = SelectPropagationCode(ActiveIdx, ExitStrs, Prompt);
%
%                            = 'Bounce'                         1
%                              'Bounce+Bellhop'                 2
%                              'Fields'                         3
%                              'Kraken'                         4
%                              'KrakenC'                        5
%                              'RAMGeo'                         6
%                              'RAMSGeo'                        7
%                              'Scooter'                        8
%                              'Scooter+Fields'                 9
%                              ExitStrs          -> empty string = 'EXIT'
%
%               ActiveIdx    = vector of indices to above switches (excluding ExitStrs) which are active
%                            = [] or omit for ALL ACTIVE
%
%               ExitStrs     = User-specified string to appear in cancel buton 
%                              ... if omit or [], = 'EXIT'
%
%               Prompt       = User-specified Prompt  
%                              ... if omit or [], = 'Select Propagation Code:'
%                              ... DO NOT OMIT IF CancelStr is passed 
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      10-12 July    2006 ... ALM    (Viva l'Italia!!)
% Revision 0.1      21    July    2006 ... ALM    (Cha-mone MFs)
%                   - 'Bellhop' is really -> 'Bounce+Bellhop' and therefpore changed 
%                    (In future may have Bellhop on its own for using bounce results - not yet)
% -----------------------------------------------------------------------------------------------------------

BaseStrs = {'Bounce'              , ...
            'Bounce+Bellhop'      , ...
            'Fields'              , ...
            'Kraken'              , ...
            'KrakenC'             , ...
            'RAMGeo'              , ...
            'RAMSGeo'             , ...            
            'Scooter'             , ...
            'Scooter+Fields'      };

if ~exist('ActiveIdx', 'var'), ActiveIdx = []; end
if isempty(ActiveIdx)
   ActiveIdx = 1:length(BaseStrs);
end
if ~exist('Prompt', 'var'), Prompt    = ''; end
if isempty(Prompt)
   Prompt    = 'Select Propagation Code:';
end
if ~exist('ExitStrs', 'var'), ExitStrs = ''; end
if isempty(ExitStrs)
   ExitStrs = 'EXIT';
end

MenuStrs = [ BaseStrs(ActiveIdx), ExitStrs ];

CodeID   = MenuStrs{menu(Prompt, MenuStrs)};
