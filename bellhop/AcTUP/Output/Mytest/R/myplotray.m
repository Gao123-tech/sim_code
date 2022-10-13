% function plotray( rayfil )

% useage: plotray( rayfil )
% where rayfil is the ray file (without the extension)
% e.g. plotray( 'foofoo' )
%
% plots the RAYfil produced by Bellhop
% MBP July 1999
rayfil='TestRay_20000.ray'



zr = 90.0;	% use this to just plot eigenrays

% open the file

fid = fopen( rayfil, 'r' );
if ( fid == -1 )
    warndlg( 'No ray file exists; you must run BELLHOP first (with ray ouput selected)', 'Warning' );
end

% read header stuff

TITLE  = fgetl(  fid );
FREQ   = fscanf( fid, '%f', 1 );
NBEAMS = fscanf( fid, '%i', 1 );
DEPTHT = fscanf( fid, '%f', 1 );
DEPTHB = fscanf( fid, '%f', 1 );

ii = findstr( TITLE(3:end), '''');   % find last quote
TITLE = deblank( TITLE(3:1:ii-1) );  % remove whitespace

% read rays

set( gca, 'YDir', 'Reverse' )   % plot with depth-axis positive down

xlabel( 'Range (m)' )
ylabel( 'Depth (m)' )
title( TITLE )
hold on

% axis limits
rmin = +1e9;
rmax = -1e9;
zmin = +1e9;
zmax = -1e9;

for ibeam = 1:NBEAMS
  alpha0 = fscanf( fid, '%f', 1 );
  nsteps = fscanf( fid, '%i', 1 );
  NumTopBnc = fscanf( fid, '%i', 1 );
  NumBotBnc = fscanf( fid, '%i', 1 );
   if isempty( nsteps ); break; end
   ray = fscanf( fid, '%f', [2 nsteps] );
   r = ray( 1, : );
   z = ray( 2, : );
   
   lincol = 'kbgrcmy';
   ii = NumBotBnc;
   ii = mod( ii, 3 ) + 1;
   plot( r, z, lincol(ii) );
   %if NumTopBnc > 1 & NumBotBnc > 1
   %  plot( r, z, 'k--' )	% hits both boundaries
   %elseif NumBotBnc > 1
   %  plot( r, z, 'r-' )	% hits bottom only
   %elseif NumTopBnc > 1
   %  plot( r, z, 'b-' )	% hits surface only
   %elseif NumTopBnc == 0 & NBotBnc == 0
   %  plot( r, z, 'b-' )
   %else
   %  plot( r, z, 'y-' )
   %end
   % update axis limits
   rmin = min( [ r rmin ] );
   rmax = max( [ r rmax ] );
   zmin = min( [ z zmin ] );
   zmax = max( [ z zmax ] );
   if ( zmin == zmax ) % horizontal ray causes axis scaling problem
      zmax = zmin + 1;
   end
   axis( [ rmin, rmax, zmin, zmax ] )
   
   if rem( ibeam, fix( NBEAMS / 10 ) ) == 0,    % flush graphics buffer every 10th ray
       drawnow
   end;
   %end
end	% next beam

fclose( fid );

hold off
zoom on
