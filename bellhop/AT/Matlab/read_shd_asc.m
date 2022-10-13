function [ pltitl, freq, atten, Pos, p ] = read_shd_asc( filename )

% open the file
fid = fopen( filename, 'r' );
if ( fid == -1 )
    warndlg( 'No shade file with that name exists; you must run a model first', 'Warning' );
end

% read

pltitl = fgetl( fid );
junk   = fgetl( fid );
freq   = fscanf( fid, '%f', 1 );
Nsd = fscanf( fid, '%i', 1 );
Nrd = fscanf( fid, '%i', 1 );
Nrr = fscanf( fid, '%i', 1 );
atten  = fscanf( fid, '%f', 1 );

Pos.s.depth  = fscanf( fid, '%f', nsd );
Pos.r.depth  = fscanf( fid, '%f', nrd );
Pos.r.range  = fscanf( fid, '%f', nrr );

isd = 1;
for i = 1:isd
   temp1   = fscanf( fid, '%f', [ 2 * nrr, nrd ] );
   i, size(temp1)
end

fclose( fid );

% joint real and imaginary parts into a complex matrix

p = temp1( 1:2:2*Nrr, : )' + sqrt( -1 ) * temp1( 2:2:2*Nrr, : )';