function ReleaseSpec = LoadReleaseSpec
%LoadReleaseSpec       Loads switches and paths which are release dependent -----------------------------------------------------
%                      Set by programming team NOT USER
%
% USAGE:       ReleaseSpec = LoadReleaseSpec(DirInfo);
%
%
% OUTPUT       ReleaseSpec  --> Lite
%                               Full
%
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      17 July         2006 ... ALM
% -----------------------------------------------------------------------------------------------------------

%DIRECTIONS: - unhighlight one (and ony one) of the modes below

% LITE:
% knocks out: * PostProcessor
%             * TransferFunction Class
%             * cTimeSeries Class
%             * Associated Plotting Tools


ReleaseSpec = 'LITE';
%ReleaseSpec = 'FULL';