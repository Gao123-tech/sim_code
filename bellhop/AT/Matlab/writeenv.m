% writeenv
% Uses the workspace variables to write out an envfil

fid = fopen( envfil, 'w' );

fprintf( fid, '%s \r\n', titleenv );
fprintf( fid, '%f \r\n', freq );
fprintf( fid, '%i \r\n', Nmedia );
fprintf( fid, '%s \r\n', topopt );

% SSP
Nssp = size( cofz, 1 );
fprintf( fid, '%i %f %f \r\n', Nssp, sigma, Depth );
fprintf( fid, '%f %f / \r\n', cofz' );

% lower halfspace
fprintf( fid, '''A'', 0.0 \r\n' );
fprintf( fid, '%f %f %f %f /  ! lower halfspace/ \r\n', Depth, cpb, csb, rhob );

%fprintf( fid, '%f %f \r\n', cmin, cmax );
%fprintf( fid, '0.0,			! RMAX (km) \r\n' );

fprintf( fid, '%i			! NSD \r\n', Nsd );
fprintf( fid, '%f  ', sd );
fprintf( fid, '! SD(1)  ... \r\n' );

fprintf( fid, '%i /		! NRD \r\n', Nrd );
fprintf( fid, '%f  ', rd );
fprintf( fid, ' /		! RD(1)  ... \r\n' );

fprintf( fid, '%i /		! NRR \r\n', Nrr );
fprintf( fid, '%f   ', rr );
fprintf( fid, '/		! RR(1)  ... \r\n' );

fprintf( fid, '%s \r\n', runtyp );
fprintf( fid, '%i \r\n', Nbeams );
fprintf( fid, '%f %f / \r\n', alpha );
fprintf( fid, '%f %f %f', step, zbox, rbox );

fclose( fid );

