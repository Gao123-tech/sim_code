function RemoveSourcePaths(DirInfo)
%RemoveSourcePaths        Removes paths to source code
%
% USAGE:       RemoveSourcePaths(DirInfo)
%
% INPUT        DirInfo      --> See AcTUP
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      17 July         2006 ... ALM
% -----------------------------------------------------------------------------------------------------------

PathList = GetSourcePaths(DirInfo);
% REM
for ii = 1:length(PathList)
   rmpath(PathList{ii});
end