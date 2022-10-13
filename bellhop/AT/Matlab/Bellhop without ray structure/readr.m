function [ rr, Nrr ] = readr( fid )

% reads in receiver ranges

Nrr = fscanf( fid, '%i', 1 );
fprintf( '\nNumber of receiver ranges = %i \n', Nrr )
fgetl( fid );

rr = fscanf( fid, '%f', Nrr );
fprintf( '%f  ', rr )

if Nrr > 2
  rr = linspace( rr( 1 ), rr( 2 ), Nrr )'; % generate vector of receiver ranges
end

rr = 1000.0 * rr; % convert km to m
fgetl( fid );
