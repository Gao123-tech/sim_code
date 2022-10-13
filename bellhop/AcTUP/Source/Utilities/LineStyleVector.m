function lstyles = LineStyleVector(N, colvec, linvec)
% LineStyle   returns an N length cell array predetermined line style strings 
%  
% USAGE:      lstyles = LineStyleVector(N)
%             lstyles = LineStyleVector(N, colvec, linvec)
%
%             use different length colvec and linvec for longer period
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
% Revision 0.0    18 April   2005 ... ALM  
%
% =======================================

if ~exist('colvec', 'var');
   colvec     = {'b', 'r', 'g', 'k', 'c', 'm', 'y' } ;
end
   
if ~exist('linvec', 'var');
   linvec     = { '-',  '--', '-.', ':'}             ;
end

ncol = length(colvec);
nlin = length(linvec);

icol = 1;
ilin = 1;

for ii = 1:N
   lstyles{ii} = [colvec{icol} linvec{ilin}];
   icol = icol+1;
   ilin = ilin+1;
   if icol > ncol, icol=1; end
   if ilin > nlin, ilin=1; end
end