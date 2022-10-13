function readbty( BotBTY, rBox )

% Read a btyfil into the workspace variables

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB TopOpt MxN
global xBot tBot nBot NbtyPts

btyfil = 'BTYFIL';

if ( BotBTY == '*' )
   disp( '*********************************' )
   disp( 'Using bottom-bathymetry file' )

   fid = fopen( btyfil, 'r' );

   NbtyPts = fscanf( fid, '%i', 1 );
   fprintf( 'Number of bathymetry points = %i \n\n', NbtyPts )
   fprintf( ' Range (km)  Depth (m) \n' )

   for ii = 1 : NbtyPts
      xBot( :, ii ) = fscanf( fid, '%f', 2 );
      fprintf( '%f    %f \n', xBot( :, ii ) );
   end

   xBot( 1, : ) = 1000.0 * xBot( 1, : );   % Convert ranges in km to m

   % compute tangent and outward-pointing normal to Bot
   tBot( 1, : ) = xBot( 1, 2:NbtyPts ) - xBot( 1, 1:NbtyPts - 1 );
   tBot( 2, : ) = xBot( 2, 2:NbtyPts ) - xBot( 2, 1:NbtyPts - 1 );
   RLenBot = sqrt( tBot( 1, : ).^ 2 + tBot( 2, : ).^ 2 );

   tBot( 1, : ) = tBot( 1, : ) ./ RLenBot;
   tBot( 2, : ) = tBot( 2, : ) ./ RLenBot;
   clear RLenBot

   nBot( 1, : ) = -tBot( 2, : );
   nBot( 2, : ) =  tBot( 1, : );
   fclose( fid );

else   % no bathymetry given, use SSP depth for flat Bot
   NbtyPts = 2;
   
   xBot( :, 1 ) = [ -rBox; depthB ];
   xBot( :, 2 ) = [  rBox; depthB ];

   tBot( :, 1 ) = [ 1.0;  0.0 ];   % tangent to Bot
   nBot( :, 1 ) = [ 0.0;  1.0 ];   % outward-pointing normal
end
