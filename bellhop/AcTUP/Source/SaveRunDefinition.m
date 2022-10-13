function RunDef = SaveRunDefinition(RunDef, filename)
%SaveRunDefinition    saves run definition depending on IP parameters
%
%                [RunDef  =] SaveRunDefinition(RunDef, filename)  ->  writes RunDef to .mat file
%
% Revision 0.0   01 December 2004 ... ALM
%                - not much to it but saves the user having to think about it 
%
% Revision 0.1   28 February 2005 ... ALM
%                - change IP order in case this has to become partially interactive in future 
%
% Revision 0.2   12 May      2005 ... ALM
%                - add LastFilename update function (see also LoadRunDefinition)
%                - save doesn't have any o/p arguments so we assume filename OK 
%                  -> not really reused in Def (o/w on load) but definitely in RunDef

if nargin ~= 2
    error('ERROR -> SaveRunDefinition: Incorrect number of parameters');
else
    RunDef.LastFilename = filename;
    % Legacy code uses Def
    Def = RunDef;
    save(filename, 'Def', '-v6', '-mat');
end