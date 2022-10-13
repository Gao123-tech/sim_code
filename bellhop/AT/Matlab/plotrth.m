function plotrth( brcfil )
% useage: plotrth( brcfil )
%
% plots the Bottom Reflection Coefficeint file used by Bellhop
% MBP May 2002

if ( strcmp( brcfil, 'BRCFIL' ) == 0 )
  brcfil = [ brcfil '.brc' ]; % append extension
end

% open the file

fid = fopen( brcfil, 'r' );
if ( fid == -1 )
   warndlg( 'BRCFIL does not exist', 'Warning' )
end

% read header stuff

Ntheta = fscanf( fid, '%i', 1 );
brcmat  = fscanf( fid, '%f', [ 3 Ntheta ] );
fclose( fid );

theta   = brcmat( 1, : );    % angles
modR    = brcmat( 2, : );      % Reflection coefficient (modulus)
phaseR  = brcmat( 3, : );    % Reflection coefficient (phase)

figure
plot( theta, modR, 'k-' )
xlabel( 'angle (degrees)' )
ylabel( '|R|' )

figure
plot( theta, 20 * log10( modR ), 'k-' )
xlabel( 'angle (degrees)' )
ylabel( 'Reflection Loss (dB)' )

figure
plot( theta, phaseR, 'k-' )
xlabel( 'angle (degrees)' )
ylabel( 'Phase (degrees)' )


%axis( [ rmin, rmax, zmin, zmax ] );
%view( 0, -90 ); % flip plot so that z-axis is pointing down
