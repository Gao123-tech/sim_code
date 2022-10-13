function plotssp_dat( filename )

% plots the sound speed profile
% useage:
%   plotssp_dat( filename )
% where filename is the output file created by plotssp.f
%
% Chris Tiemann/SAIC
% May 2001

disp('This function plots the internal soundspeed profile used in BELLHOP') 
disp('calculations as created by  PLOTSSP.BAT  from DOS.  To view the')
disp('user defined soundspeed profile read by BELLHOP use the Matlab')
disp('function plotssp.m.')

%Open the file
fid = fopen( filename, 'r' );

titlestr=deblank(fgetl(fid));
titlestr=titlestr(9:length(titlestr)-1);

%Skip through header lines
for i=1:4,fgetl(fid);end

ssp=[];
s=fgetl(fid);

%Read to end of file
while s~=-1 
   ssp=[ssp; str2num(s)];
   s=fgetl(fid);
end

%Plot results
figure
plot(ssp(:,2),ssp(:,1))
set(gca,'YDir','reverse');
title(titlestr)
xlabel('Soundspeed [m/s]');ylabel('Depth [m]')

fclose all;
