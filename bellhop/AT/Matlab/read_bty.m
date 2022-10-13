% readbty
% Read a bathymetry into the workspace variables

fid = fopen( btyfil, 'r' );

Npts 	= fscanf( fid, '%i', 1 );
rngdep = fscanf( fid, '%e', [ 2, Npts ] );
rngdep = rngdep';

fclose( fid );

