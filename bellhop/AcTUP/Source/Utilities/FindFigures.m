function [fhdls, current, next, unused] = FindFigures()
%FindFigures    Finds extant figures and can also return a bunch rarely useful bits of information
%
%USAGE      [fhdls, current, next, unnused] = FindFigures();
%
%OP          fhdls    vector of used figure numbers
%            current  current figure handle (gcf)  
%                     ... = 0 if none
%            next     next available figure number > max(fhdls)
%            unused   set of figure handles < max(fhdls) which are currently unused  
%                     ... = [] if no gaps in set or no figures at all
%
%
% Amos L Maggi
% Centre for Marine Science and Technology
% Physics Department
% Curtin University
% Perth, Western Australia
% a.maggi@curtin.edu.au
%
% Revision History
% ================
% Revision 0.0    27 January  2005 ... ALM  
% Revision 0.01   12 April    2006 ... ALM  
%                 - fixed incorrect spelling of "unused" in function def 


fhdls = findobj('Type', 'figure');
if isempty(fhdls)
    next     = 1 ;
    current  = 0 ;
    unused   = [];
else
    next           = max(fhdls)+1 ;
    current        = gcf          ;
    unused         = 1:next-1     ;
    unused(fhdls)  = []           ;
end
