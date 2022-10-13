function plotbty( btyfil )

% useage: plotbty( btyfil )
% where btyfil is the BaThYmetry file (without the extension)
% e.g. plotbty( 'foofoo' )
%
% plots the BaThymetrY file used by Bellhop
% MBP July 1999

if ( strcmp( btyfil, 'BTYFIL' ) == 0 )
  btyfil = [ btyfil '.bty' ]; % append extension
end

% open the file

fid = fopen( btyfil, 'r' );
if ( fid == -1 )
   warndlg( 'BTYFIL does not exist', 'Warning' )
end

% read header stuff

Nbtypts = fscanf( fid, '%i', 1 );
btymat  = fscanf( fid, '%f', [ 2 Nbtypts ] );
fclose( fid );

r = 1000 * btymat( 1, : );    % convert to meters
z = btymat( 2, : );

hold on
plot( r, z, 'k-' )
xlabel( 'Range (m)' )
ylabel( 'Depth (m)' )

zmin = min( z );
zmax = max( z );

r( Nbtypts + 1 ) = r( Nbtypts );
z( Nbtypts + 1 ) = zmax + 0.1 * ( zmax - zmin );
r( Nbtypts + 2 ) = r( 1 );
z( Nbtypts + 2 ) = zmax + 0.1 * ( zmax - zmin );

fill( r, z, [ 0.5 0.3 0.1 ] );

rmin = min( r );
rmax = max( r );
zmin = min( z );
zmax = max( z );

%axis( [ rmin, rmax, zmin, zmax ] );
set( gca, 'YDir', 'Reverse' )   % plot with depth-axis positive down
