function [statvec, strvec] = DescStats(y)
%DescStats ... Descriptive Statistics on input vector / series y
%
%USAGE:  [statvec, strvec] = DescStats(y)
%
% give it a crack
% 
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision 0        132 September 2005 ... ALM

statvec(1) = min(y)     ; strvec{1} = 'min'    ;
statvec(2) = max(y)     ; strvec{2} = 'max'    ;
statvec(3) = mean(y)    ; strvec{3} = 'mean'   ;
statvec(4) = median(y)  ; strvec{4} = 'median' ;
statvec(5) = std(y)     ; strvec{5} = 'std'    ;
statvec(6) = var(y)     ; strvec{6} = 'var'    ;

