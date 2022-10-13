% writebty
% Read a bathymetry into the workspace variables

fid = fopen( btyfil, 'w' );

fprintf( fid, '%i \r\n', Npts );
fprintf( fid, '%f %f \r\n', rngdep' );

fclose( fid );

