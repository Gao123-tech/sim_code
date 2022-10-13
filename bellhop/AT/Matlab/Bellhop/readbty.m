function readbty( btyfil, BotBTY, depthB, rBox )

% Read a btyfil into the workspace variables

global Bdry
global xBot tBot nBot NbtyPts

if ( BotBTY == '*' )
    disp( '*********************************' )
    disp( 'Using bottom-bathymetry file' )

    fid = fopen( btyfil, 'r' );

    NbtyPts = fscanf( fid, '%i', 1 );
    fprintf( 'Number of bathymetry points = %i \n\n', NbtyPts )
    fprintf( ' Range (km)  Depth (m) \n' )

    for ii = 1 : NbtyPts
        xBot( :, ii+1 ) = fscanf( fid, '%f', 2 );
        fprintf( '%f    %f \n', xBot( :, ii+1 ) );
        if ( xBot( 2, ii ) > depthB )
            error( 'Bathymetry goes below last tabulated SSP value' )
        end
    end

    xBot( 1, : ) = 1000.0 * xBot( 1, : );   % Convert ranges in km to m
    
    % extend the bathymetry to +/- infinity in a piecewise constant fashion
    NbtyPts = NbtyPts + 2;
    xBot( 1, 1 )       = -inf;
    xBot( 2, 1 )       = xBot( 2,    2);
    xBot( 1, NbtyPts ) = inf;
    xBot( 2, NbtyPts ) = xBot( 2, end );
    
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

    xBot( :, 1 ) = [ -rBox; Bdry.Bot.depth ];
    xBot( :, 2 ) = [  rBox; Bdry.Bot.depth ];

    tBot( :, 1 ) = [ 1.0;  0.0 ];   % tangent to Bot
    nBot( :, 1 ) = [ 0.0;  1.0 ];   % outward-pointing normal
end
