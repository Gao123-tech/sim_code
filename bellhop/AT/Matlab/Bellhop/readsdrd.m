function Pos = readsdrd( fid )

% reads in source depths and receiver depths
% Variable 'Pos' is a structure:
% Pos.r.depth = vector of receiver depths
% Pos.Nrd     = number of receiver depths
% Pos.s.depth = vector of source depths
% Pos.Nsd     = number of source depths
   
% source info
Pos.Nsd = fscanf( fid, '%i', 1 );
fprintf( '\nNumber of sources   = %i \n', Pos.Nsd )
fgetl( fid );

Pos.s.depth = fscanf( fid, '%f', Pos.Nsd );
fprintf( '%8.2f  \n', Pos.s.depth )
fgetl( fid );

% receiver info
Pos.Nrd = fscanf( fid, '%i', 1 );
fprintf( '\nNumber of receivers = %i \n', Pos.Nrd )
fgetl( fid );

Pos.r.depth = fscanf( fid, '%f', Pos.Nrd );
fprintf( '%8.2f  ', Pos.r.depth )
disp( '  ' )

if Pos.Nrd > 2
  Pos.r.depth = linspace( Pos.r.depth( 1 ), Pos.r.depth( 2 ), Pos.Nrd ); % generate vector of receiver depths
end
fgetl( fid );
fprintf( '\n' )