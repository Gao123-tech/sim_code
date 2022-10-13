function sdrdr( fid )

% reads in source depths and receiver depths

global sd Nsd rd Nrd r Nr
   
Nsd = fscanf( fid, '%i', 1 );
fprintf( '\nNumber of sources   = %i \n', Nsd )
fgetl( fid );
sd = fscanf( fid, '%f', Nsd );
fprintf( '%8.2f  \n', sd )
fgetl( fid );

Nrd = fscanf( fid, '%i', 1 );
fprintf( '\nNumber of receivers = %i \n', Nrd )
fgetl( fid );
rd = fscanf( fid, '%f', Nrd );
fprintf( '%8.2f  ', rd )
disp( '  ' )
if Nrd > 2
  rd = linspace( rd( 1 ), rd( 2 ), Nrd ); % generate vector of receiver depths
end
fgetl( fid );
