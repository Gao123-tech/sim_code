function plotray( filename )

% plots a BELLHOP ray file

zr = 90.0;	% use this to just plot eigenrays

% open the file

fid = fopen( filename, 'r' );

% read header stuff

TITLE  = fgetl(  fid );
FREQ   = fscanf( fid, '%f', 1 );
NBEAMS = fscanf( fid, '%i', 1 );
DEPTHT = fscanf( fid, '%f', 1 );
DEPTHB = fscanf( fid, '%f', 1 );

% read rays

view( 0, -90 ); % flip plot so that z-axis is pointing down
xlabel( 'Range (km)' )
ylabel( 'Depth (m)' )
%title( deblank( TITLE ) )
hold on

for ibeam = 1:NBEAMS
  nsteps = fscanf( fid, '%i', 1 );
  [ibeam nsteps]
   if isempty( nsteps ) break; end
   ray = fscanf( fid, '%f', [2 nsteps] );
   ray( 1, : ) = ray( 1, : ) / 1000;    % convert to km
   
   %if abs( ray( 2, nsteps ) - zr ) < 2	% use this to just plot eigenrays
   % plot, using a different color for each ray class
   
   surbnc = find( ray( 2, : ) <= DEPTHT );
   botbnc = find( ray( 2, : ) >= DEPTHB );
   
   if size( surbnc, 2 ) > 1 & size( botbnc, 2 ) > 1
     plot( ray( 1, : ), ray( 2, : ), 'k:' )	% hits both boundaries
   elseif size( botbnc, 2 ) > 1
     plot( ray( 1, : ), ray( 2, : ), 'r-' )	% hits bottom only
   elseif size( surbnc, 2 ) > 1
     plot( ray( 1, : ), ray( 2, : ), 'k-' )	% hits surface only
   elseif size( surbnc, 2 ) == 0 & size( botbnc, 2 ) == 0
     plot( ray( 1, : ), ray( 2, : ), 'b-' )
   else
     plot( ray( 1, : ), ray( 2, : ), 'y-' )
   end
   %end
end	% next beam

fclose( fid );
axis( [ 0 150 0 4000 ] )
zoom on



