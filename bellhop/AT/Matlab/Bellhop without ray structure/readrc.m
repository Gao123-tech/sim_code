function readrc( TopRC, BotRC )
% reads in the top and bottom reflection coefficients

global thetaBot RBot phiBot NBotPts thetaTop RTop phiTop NTopPts

if ( TopRC == 'F' )
  disp( 'Reading top    reflection coefficient' )
  fid = fopen( 'TRCFIL', 'r' );
  NBotPts = fscanf( fid, '%i', 1 );
  fprintf( 'Number of points in top    reflection coefficent = %i \n', NTopPts )
  for ii = 1 : NTopPts
    thetaBot( ii ) = fscanf( fid, '%f', 1 );
    RBot(     ii ) = fscanf( fid, '%f', 1 );
    phiBot(   ii ) = fscanf( fid, '%f', 1 );
  end
else
  thetaBot = 0;
  RBot     = 0;
  phiBot   = 0;
  NBotPts  = 0;
end

if ( BotRC == 'F' )
  disp( 'Reading bottom reflection coefficient' )
  fid = fopen( 'BRCFIL', 'r' );
  NBotPts = fscanf( fid, '%i', 1 );
  fprintf( 'Number of points in bottom reflection coefficent = %i \n', NBotPts )
  for ii = 1 : NBotPts
    thetaBot( ii ) = fscanf( fid, '%f', 1 );
    RBot(     ii ) = fscanf( fid, '%f', 1 );
    phiBot(   ii ) = fscanf( fid, '%f', 1 );
  end
else
  thetaTop = 0;
  RTop     = 0;
  phiTop   = 0;
  NTopPts  = 0;
end