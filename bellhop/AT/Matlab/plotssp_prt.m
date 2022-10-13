function plotssp_prt( filename )

% useage:
%   plotssp_prt( filename )
% where filename is complete with extension (.prt)
%
% Chris Tiemann/SAIC
% May 2001

disp('This function plots a single user defined soundspeed profile') 
disp('that is read by BELLHOP and  found in the *.prt output file.')
disp(' ')
disp('To see the internal soundspeed profile used in BELLHOP calculations')
disp('run  PLOTSSP.BAT  from DOS to create a plotssp.dat file and view it with')
disp('the Matlab function plotssp2.m.')

%Open the file
fid = fopen( filename, 'r' )

titlestr = deblank( fgetl( fid ) );

%Search for start of profile
s = titlestr;
while ~strcmp( s, 'Sound speed profile:' )
   s = fgetl( fid )
end

ssp = [];
%Read to end of profile
while ~isempty(s)
   s = fgetl( fid );
   ssp = [ ssp; str2num( s ) ]
end

%Plot results

figure
plot( ssp( :, 2 ), ssp( :, 1 ) )
set( gca, 'YDir', 'reverse' )
title( titlestr )
xlabel( 'Sound speed (m/s)')
ylabel( 'Depth (m)' )

fclose all
