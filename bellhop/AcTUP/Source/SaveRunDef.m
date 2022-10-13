function [vout1, vout2] = SaveRunDef(Def, DirInfo, mode)
%SaveRunDef      Saves Run Definition File (previously lSaveRunDef ~ AcToolboxFrontEnd.m) using DirInfo -------------------
%
% USAGE:       [Def, DirInfo]       = SaveRunDef(Def, DirInfo);
%
%              [Def, DirInfo]       = SaveRunDef(Def, DirInfo, 'retStruct');
% 
% alt:         [file, path]         = SaveRunDef(DirInfo, Def, 'retFile'  );  (where file, path indicate save location)
%
%
% See AcTUP for object definitions
% Use SaveRunDefinition if not using DirInfo and related support not required
% Revision History
% ================
% Revision 0.0      xx ???????   ???? 
%                   - Originally incorporated in AcToolboxFrontEnd.m
% Revision 1.0      05 July      2006 ... ALM
%                   - Parsed into function
% -----------------------------------------------------------------------------------------------------------

Here      =  pwd;
Status    =  1  ;
if ~exist('mode', 'var'), mode = []          ; end
if isempty( mode )      , mode = 'retStruct' ; end

switch upper(mode)
   case 'RETSTRUCT'
      vout1    = Def     ;
      vout2    = DirInfo ;      
   otherwise 
      vout1    = [] ;
      vout2    = [] ;
end

[lastpath, lastname] = StripPath(Def.LastFilename);
if ~exist(lastpath, 'dir')
   lastpath = DirInfo.LastRunDef;
   if ~exist(lastpath, 'dir')
      lastpath = DirInfo.RunDef;
      if ~exist(lastpath, 'dir')
         Status = LibMakeDirectory(lastpath);
      end
   end
end

if ~Status
    uiwait(warndlg(['Couldn''t find last directory or make directory: ' DirInfo.RunDef ', run definition not saved']));
    file = [];
    path = [];
else
    cd(lastpath);
    [file, path] = uiputfile('*.mat', 'Environment definition file', lastname);
    cd(Here);
    if file ~= 0
        filename = [path file];
        % save([Path File], 'Def');   %saves Def structure
        % replace with new function
        Def = SaveRunDefinition(Def, filename); % this also sets Def.LastFilename
        DirInfo.LastRunDef = path;
        % write o/p if OK
    else
       file = [];
       path = [];
    end
end
switch upper(mode)
   case 'RETSTRUCT'
      vout1    = Def     ;
      vout2    = DirInfo ;      
   otherwise 
      vout1    = file    ;
      vout2    = path    ;
end
