function PropagationCodeHelp()
%PropagationCodeHelp       Help dialog - useful for SelectPropagationCode and related functions ----------------------------------
%
% USAGE:                  PropagationCodeHelp
%
% I/O                     DirInfo, Def --> See AcTUP
%                    
% Revision History
% ================
% Revision 0.0      24 July      2006 ... ALM 
% -------------------------------------------------------------------------------------------------------------------------------
FALSE = 0;
TRUE  = 1;

HelpStrs = {['Bounce'              , '  ...  Reflection coefficient calc'], ...
            ['Bounce+Bellhop'      , '  ...  + ray tracing & various TL calcs']...
            ['Fields'              , '  ...  TL calc from existing Scooter OP by FFT (Fast Field)'] ...
            ['Kraken'              , '  ...  Normal modes search + TL calc'] ...
            ['KrakenC'             , '  ...  Complex normal modes search + TL calc'] ...
            ['RAMGeo'              , '  ...  TL via parabolic equation solution ~ fluid substrate'] ...
            ['RAMSGeo'             , '  ...  TL via parabolic equation solution ~ elastic substrate'] ...            
            ['Scooter'             , '  ...  Finds depth-dependent Green''s functions'] ...
            ['Scooter+Fields'      , '  ...  Steps through Scooter then Fields automatically']};
         
%
hdl = helpdlg(HelpStrs,'Available Propagation Codes ...');
set(hdl,  'WindowStyle', 'modal');
%set(hdl,  'Units', 'normalized', 'Position', [.25 .25 .5 .5]);  % won't work - need to set textbox param if locatable
waitfor(hdl);


