function SAMOpt = BuildSAMOpt(RunID)
%BuildSAMOpt      Used for cuilding menu option for selecting active propagation model 
%
% USAGE:                  SAMOpt = BuildSAMOpt(RunID);
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
SAMBase = 'Select Active Code';
SAMOpt  = [SAMBase ' ~ ', char(RunID{1})];
