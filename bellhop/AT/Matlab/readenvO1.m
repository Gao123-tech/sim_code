% readenv
% Read an envfil into the workspace variables

fid = fopen( envfil, 'r' );

titleenv = fgetl( fid );
freq     = fscanf( fid, '%f [...]', 1 ); fgetl( fid );
Nmedia   = fscanf( fid, '%i', 1 ); fgetl( fid );
topopt   = fgetl( fid );

% SSP

Npts 	= fscanf( fid, '%i', 1 );
sigma = fscanf( fid, '%f', 1 );
Depth = fscanf( fid, '%f', 1 ); fgetl( fid );

clear cofz;

for ii = 1:999
  cofz( :, ii ) = fscanf( fid, '%f', 2 );
  fgetl( fid );
  if ( cofz( 1, ii ) == Depth ); break; end
end
cofz = cofz';

% lower halfspace
botopt = fgetl( fid );

if ( botopt(2:2) == 'A' )
Depth	= fscanf( fid, '%f', 1 )
cpb	= fscanf( fid, '%f', 1 )
csb	= fscanf( fid, '%f', 1 )
rhob	= fscanf( fid, '%f', 1 )
fgetl( fid );
end

%fscanf( fid, '%f %f', cmin, cmax );
%fscanf( fid, '0.0,			! RMAX (km)' );

Nsd = fscanf( fid, '%i', 1 );
fgetl( fid );
sd = fscanf( fid, '%f', Nsd );
fgetl( fid );

Nrd = fscanf( fid, '%i', 1 );
fgetl( fid );
rd = fscanf( fid, '%f  ', Nrd );
fgetl( fid );

Nrr = fscanf( fid, '%i', 1 );
fgetl( fid );
rr = fscanf( fid, '%f', Nrr );
fgetl( fid );

runtyp = fgetl( fid );
Nbeams = fscanf( fid, '%i', 1 );
fgetl( fid );
alpha = fscanf( fid, '%f', Nbeams );
fgetl( fid );

step = fscanf( fid, '%f', 1 );
zbox = fscanf( fid, '%f', 1 );
rbox = fscanf( fid, '%f', 1 );

fclose( fid );

