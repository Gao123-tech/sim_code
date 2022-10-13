function GroupName = BuildAndTestOutputDir(Def, DirInfo, Interactive)
%BuildAndTestOutputDir         Builds and tests group filename base for multi-frequency propagation runs from RunDefinition parameters
%
%USAGE                  GroupName     =  BuildAndTestOutputDir(Def, DirInfo[, Interactive])
%
%OUTPUT                 GroupName     =  Common root for the path+filename of the individual frequency output files by AcTUP
%                                     = [] on failure
%
%INPUT                  Def, DirInfo  -> see AcTUP for definitions
%                       Interactive   =  1             for user interaction required 
%                                     =  0  [Default]  user-interaction not possible (e.g. batch mode)
%                                        This determines how the function displays warnings
%                                        
%                           
% Revision History
% ================
% Revision 0.0      11 July      2006 ... ALM (Viva l'Italia!!!)
% Revision 0.1      21 July      2006 ... ALM (Cham-one MF)
%                   -  added prop model dependent subfolder to OP path
% -------------------------------------------------------------------------------------------------------------------------------

FALSE = 0;

if ~exist('Interactive', 'var'), Interactive = FALSE; end
% CREATE GROUP NAME
% Group name isn't directly used for "Fields" ONLY since 
% a) when run by itself the user selects existing .grn files and the shade files are placed in the same directory 
% b) when run with Scooter the selected mode is 'Scooter+Fields' and so the appropriate sub directory is added

% Add sub folder to OP path to reflect model used
ModSub = '';
switch Def.RunID{1}
   case 'Fields'
      %NOT REQUIRED
   case {'Scooter', 'Scooter+Fields'}   
      ModSub = ['Scooter&Fields', '\'];
   case {'Bellhop', 'Bounce'}
      ModSub = ['Bounce&Bellhop', '\'];
   otherwise
      ModSub = [Def.RunID{1}    , '\'];
end
GroupName = [DirInfo.MainWork Def.SubDir ModSub Def.FPrefix];    %Includes path
% -> Def.FPrefix can contain subpath also so extract target directory rather than build it
ThisDir   = StripPath(GroupName, 'p');
% TEST TARGET DIR
if ~exist(ThisDir, 'dir');
   % ok try building dir
    DirStatus = LibMakeDirectory(ThisDir);
    if ~DirStatus
        if Interactive
            uiwait(warndlg(['Unable to create target directory: ' ThisDir]));
        else
            disp(['Unable to create target directory: ' ThisDir]);
        end
        % return on fail
        GroupName = [];        
        return;
    end
end
% TEST WRITING TO GROUPNAME
% -> Test as default (binary) as most files are in this format - maybe all of them should be !
% -> This maybe should also occur at setup to simplify things for the user but should not set a flag or prevent exit from setup
%    The user may be setting up on a machine that isn't the target machine and should be allowed to continue if they wish
testname       = [GroupName, '_writetest.bin'];
[pname, fname] = StripPath(testname); 
fid            = fopen(testname, 'w');
if fid < 0
   if Interactive
      uiwait(warndlg({'Unable to create test file: ', ...
                     fname, ...
                     '@'  , ...
                     pname, ...
                     ''   , ...  
                     'Modify model-independent parameters or', ...
                     'check directory attributes (e.g. read only?)'
                     }, ...
             'Problem with Output Path or Filename Prefix'));
   else
      disp('Unable to create test file: ');
      disp(testfile);
      disp('... Modify model-independent parameters or check directory attributes (e.g. read only?)');
   end
   % return on fail
   GroupName = [];
   return;
else
   %remove test file
   fclose(fid)       ;
   delete(testname)  ;
end
