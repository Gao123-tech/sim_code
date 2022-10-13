function readati( atifil, TopATI, depthT, rBox )

% Read an atifil into the workspace variables

global Bdry
global xTop tTop nTop NatiPts

if ( TopATI == '*' )
    disp( '*********************************' )
    disp( 'Using top-altimetry file' )

    fid = fopen( atifil, 'r' );

    NatiPts = fscanf( fid, '%i', 1 );
    fprintf( 'Number of altimetry points = %i \n\n', NatiPts )
    fprintf( ' Range (km)  Depth (m) \n' )

    for ii = 1 : NatiPts
        xTop( :, ii+1 ) = fscanf( fid, '%f', 2 );
        fprintf( '%f    %f \n', xTop( :, ii+1 ) );
        if ( xTop( 2, ii ) < depthT )
            error( 'Altimetry goes above first tabulated SSP value' )
        end
    end

    xTop( 1, : ) = 1000.0 * xTop( 1, : );   % Convert ranges in km to m

    % extend the bathymetry to +/- infinity in a piecewise constant fashion
    NatiPts = NatiPts + 2;
    xTop( 1, 1 )       = -inf;
    xTop( 2, 1 )       = xTop( 2,    2);
    xTop( 1, NatiPts ) = inf;
    xTop( 2, NatiPts ) = xTop( 2, end );
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

    xTop( :, 1 ) = [ -rBox; Bdry.Top.depth ];
    xTop( :, 2 ) = [  rBox; Bdry.Top.depth ];

    tTop( :, 1 ) = [ 1.0;  0.0 ];   % tangent to top
    nTop( :, 1 ) = [ 0.0; -1.0 ];   % outward-pointing normal
end