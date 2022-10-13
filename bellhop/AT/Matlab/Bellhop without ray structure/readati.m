function readati( TopATI, rBox )

% Read an atifil into the workspace variables

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB TopOpt MxN
global xTop tTop nTop NatiPts

atifil = 'ATIFIL';

if ( TopATI == '*' )
  disp( '*********************************' )
  disp( 'Using top-altimetry file' )
  
  fid = fopen( atifil, 'r' );
  
  NatiPts = fscanf( fid, '%i', 1 );
  fprintf( 'Number of altimetry points = %i \n\n', NatiPts )
  fprintf( ' Range (km)  Depth (m) \n' )
  
  for ii = 1 : NatiPts
    xTop( :, ii ) = fscanf( fid, '%f', 2 );
    fprintf( '%f    %f \n', xTop( :, ii ) );
  end
  
  xTop( 1, : ) = 1000.0 * xTop( 1, : );   % Convert ranges in km to m
  
  % compute tangent and outward-pointing normal to top
  tToP( 1, : ) = xTop( 1, 2:NatiPts ) - xTop( 1, 1:NatiPts - 1 );
  tTop( 2, : ) = xTop( 2, 2:NatiPts ) - xTop( 2, 1:NatiPts - 1 );
  RLenTop = sqrt( tTop( 1, : ).^ 2 + tTop( 2, : ).^ 2 );
  
  tTop( 1, : ) = tTop( 1, : ) ./ RLenTop;
  tTop( 2, : ) = tTop( 2, : ) ./ RLenTop;
  clear RLenTop
  
  nTop( 1, : ) =  tTop( 2, : );
  nTop( 2, : ) = -tTop( 1, : );
  fclose( fid );
  
else   % no bathymetry given, use SSP depth for flat top
  NatiPts = 2;
  
  xTop( :, 1 ) = [ -rBox; depthT ];
  xTop( :, 2 ) = [  rBox; depthT ];
  
  tTop( :, 1 ) = [ 1.0;  0.0 ];   % tangent to top
  nTop( :, 1 ) = [ 0.0; -1.0 ];   % outward-pointing normal
end