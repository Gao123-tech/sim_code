function plotati( atifil )

% useage: plotati( atifil )
% where atifil is the Altimetry file (without the extension)
% e.g. plotati( 'foofoo' )
%
% plots the AlTImetrY file used by Bellhop
% MBP July 1999

if ( strcmp( atifil, 'ATIFIL' ) == 0 )
  atifil = [ atifil '.ati' ]; % append extension
end

% open the file

fid = fopen( atifil, 'r' );
if ( fid == -1 )
   warndlg( 'ATIFIL does not exist', 'Warning' )
end

% read header stuff

Natipts = fscanf( fid, '%i', 1 );
atimat  = fscanf( fid, '%f', [ 2 Natipts ] );
fclose( fid );

r = 1000 * atimat( 1, : );    % convert to meters
z = atimat( 2, : );

hold on
plot( r, z, 'k-' )
xlabel( 'Range (m)' )
ylabel( 'Depth (m)' )

zmin = min( z );
zmax = max( z );

r( Natipts + 1 ) = r( Natipts );
z( Natipts + 1 ) = zmin - 0.1 * ( zmax - zmin );
r( Natipts + 2 ) = r( 1 );
z( Natipts + 2 ) = zmin - 0.1 * ( zmax - zmin );

[ r z]
fill( r, z, [ 0.7 0.7 1.0 ] );

rmin = min( r );
rmax = max( r );
zmin = min( z );
zmax = max( z );

%axis( [ rmin, rmax, zmin, zmax ] );
set( gca, 'YDir', 'Reverse' )   % because view messes up the zoom feature
 