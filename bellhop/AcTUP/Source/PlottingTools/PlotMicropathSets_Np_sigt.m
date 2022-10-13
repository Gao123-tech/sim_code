function PlotMicropathSets_Np_sigt(pname)

DfltDir = [];
FALSE = 0;
TRUE  = 1;
here  = pwd;
if exist('DfltDir','dir')
   NewDir = DfltDir;
else
   NewDir = here;
end

if nargin >= 1
   if exist(DfltDir, 'dir')
      cd(DfltDir);
   end
end
%pname = uigetdir(NewDir, 'Select dir from which to to extract all .mpout files');
%cd(here);
%if ~ischar(pname)
%   return;
%end
%pname = 'd:\AcT\AcT_UI_Dev\Output\404\micropath\9002_r3514';
%pname = 'S:\CMST\Projects\400-449\404 DSTO - Propagation modelling for marine mammal impacts\Work\Data\9008\r=05101m\Micropath\data files'; 

files = dir([pname, '\*.mpout']);

names  = {files.name};
nfiles = length(names);

% for ii = 1:nfiles,
for ii = 1:nfiles,
   disp(ii);
   % read and collect all npaths and sigmat
   fname = [pname, '\', names{ii}];
   load('-mat', fname);
   % shite the op name is wrong - change instead of changing code
   MPout = TempOP;
   clear TempOP;
   npaths(ii)    = MPout.MC(1,1).npaths;
   sigmat(ii)    = MPout.MC(1,1).sigmat;
   PeakPos(ii,1) = MPout.MC(1,1).RxStats.PeakPos.mean(end) / MPout.RxStats.PeakPos.data(1) ;
   PeakPos(ii,2) = MPout.MC(1,1).RxStats.PeakPos.std( end) / MPout.RxStats.PeakPos.data(1) ;
   IntSqr( ii,1) = MPout.MC(1,1).RxStats.IntSqr.mean( end) / MPout.RxStats.IntSqr.data( 1) ;
   IntSqr( ii,2) = MPout.MC(1,1).RxStats.IntSqr.std(  end) / MPout.RxStats.IntSqr.data( 1) ;
   if ii == 1,
      N             = length(MPout.MC.H);
   end
   clear MPout;
end

% cull and sort

%---> stupid unique picks differences of 10^-18 !! ... the following is a stupid fix - where did you put the precision adjustment Mathworks?
%Np = unique(npaths);
%st = unique(sigmat);
for ii = 1:nfiles
   npathsc{ii} = num2str(npaths(ii));
   sigmatc{ii} = num2str(sigmat(ii));
end
npathsc = unique(npathsc);
sigmatc = unique(sigmatc);
for ii = 1:length(npathsc),
   Np(ii) = str2num(npathsc{ii});
end
for ii = 1:length(sigmatc),
   st(ii) = str2num(sigmatc{ii});
end
%-------------------------------------------------------------

melblanks  = zeros(length(Np), length(st), 2);
matPeakPos = melblanks;
matIntSqr  = melblanks;
clear blanks;

% loop again and place stats into matricies
for ii = 1:nfiles,
   jj = find(abs(Np-npaths(ii)) < 10*eps);
   kk = find(abs(st-sigmat(ii)) < 10*eps);   
   matPeakPos(jj,kk,1:2) = PeakPos(ii,1:2) ;
   matIntSqr( jj,kk,1:2) = IntSqr( ii,1:2) ;
end

%pcolor(xvec(cols), yvec(rows), a(x,y))
figvec(1)=figure;
subplot(2,1,1)
pcolor(st, Np, matPeakPos(:,:,1));
tstr = ['Peak Pressure (+) \mu - nMc = ',num2str(N)];
title(tstr);
xlabel('\sigma_t (s)');
ylabel('N_p_a_t_h_s');
subplot(2,1,2)
pcolor(st, Np, matPeakPos(:,:,2));
tstr = ['Peak Pressure (+) \sigma - nMc = ',num2str(N)];
title(tstr);
xlabel('\sigma_t (s)');
ylabel('N_p_a_t_h_s');

figvec(1)=figure;
subplot(2,1,1)
pcolor(st, Np, matIntSqr(:,:,1));
tstr = ['Integrated Square Pressure \mu - nMc = ',num2str(N)];
title(tstr);
xlabel('\sigma_t (s)');
ylabel('N_p_a_t_h_s');
subplot(2,1,2)
pcolor(st, Np, matIntSqr(:,:,2));
tstr = ['Integrated Square Pressure \sigma - nMc = ',num2str(N)];
title(tstr);
xlabel('\sigma_t (s)');
ylabel('N_p_a_t_h_s');
