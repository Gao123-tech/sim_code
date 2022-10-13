function readrc( trcfil, brcfil, TopRC, BotRC )
% reads in the top and bottom reflection coefficients

global thetaBot RBot phiBot NBotPts thetaTop RTop phiTop NTopPts

if ( TopRC == 'F' )
    disp( 'Reading top    reflection coefficient' )
    fid = fopen( trcfil, 'r' );
    NTopPts = fscanf( fid, '%i', 1 );
    fprintf( 'Number of points in top    reflection coefficent = %i \n', NTopPts )
    thetaTop = zeros( NTopPts, 1 );
    RTop     = zeros( NTopPts, 1 );
    phiTop   = zeros( NTopPts, 1 );
    for ii = 1 : NTopPts
        thetaTop( ii ) = fscanf( fid, '%f', 1 );
        RTop(     ii ) = fscanf( fid, '%f', 1 );
        phiTop(   ii ) = fscanf( fid, '%f', 1 );
    end
    fclose( fid );
    phiTop = pi / 180 * phiTop;   ! convert to radians

else
    thetaTop = 0;
    RTop     = 0;
    phiTop   = 0;
    NTopPts  = 0;
end

if ( BotRC == 'F' )
    disp( 'Reading bottom reflection coefficient' )
    fid = fopen( brcfil, 'r' );
    NBotPts = fscanf( fid, '%i', 1 );
    fprintf( 'Number of points in bottom reflection coefficent = %i \n', NBotPts )
    thetaBot = zeros( NBotPts, 1 );
    RBot     = zeros( NBotPts, 1 );
    phiBot   = zeros( NBotPts, 1 );
    for ii = 1 : NBotPts
        thetaBot( ii ) = fscanf( fid, '%f', 1 );
        RBot(     ii ) = fscanf( fid, '%f', 1 );
        phiBot(   ii ) = fscanf( fid, '%f', 1 );
    end
    fclose( fid );
    phiBot = pi / 180 * phiBot;   ! convert to radians

else
    thetaTop = 0;
    RTop     = 0;
    phiTop   = 0;
    NTopPts  = 0;
end

