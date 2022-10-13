function bellhop( filename )

% Beam-tracing code for ocean acoustics problems
% Based on the earlier Fortran version
% MBP Dec. 30, 2001

clear global
tic
global zSSPV cSSPV czV

if ( isempty( filename ) )
   warndlg( 'No envfil has been selected', 'Warning' );
end

global xv TrayV pV qV tauV Rfa cV Nsteps omega depthT depthB rhoB rhoT cpT cpB csT csB TopOpt MxN
global sd Nsd rd Nrd rr Nrr
global alpha Nbeams
global xTop tTop nTop NatiPts
global xBot tBot nBot NbtyPts
global thetaTop RTop phiTop NTopPts thetaBot RBot phiBot NBotPTS
global U NArr DelArr AArr AngArr MxNArr

envfil = [ filename '.env' ];
shdfil = [ filename '.mat' ];    % output file name (pressure)
arrfil = [ filename '.mat' ];    % output file name (arrivals)

[ PlotTitle, freq, TopOpt, depthT, cpT, csT, rhoT, BotOpt, depthB, cpB, csB, rhoB, fid ] = readenv( envfil );    % read in the environmental file
readsdrd( fid )    % read in the source and receiver depths
[ rr, Nrr ] = readr(    fid );    % read in receiver ranges

[ Isingl, Nimage, Ibwin, deltas, MxN, zBox, rBox, epmult, rLoop, RunType, BeamType ] = read_bell( fid, freq, depthB, depthT );

ibeams = 1 : Nbeams;   % select beams to trace
Nbeams = length( ibeams );
xv    = zeros( Nbeams, 2 , MxN );
TrayV = zeros( Nbeams, 2 , MxN );
pV    = zeros( Nbeams, 2 , MxN );
qV    = zeros( Nbeams, 2 , MxN );
Rfa   = zeros( Nbeams, 2 , MxN );
tauV  = zeros( Nbeams,     MxN );
cV    = zeros( Nbeams,     MxN );

TopATI = TopOpt(4:4);
readati( TopATI, rBox );    % read in the altimetry  file

BotBTY = BotOpt(2:2);
readbty( BotBTY, rBox );    % read in the bathymetry file

readrc( TopOpt(2:2), BotOpt(1:1) );   % READ Reflection Coefficients (top and bottom)

if ( ismember( 'A', upper( RunType(1:1) ) ) )
   MxNArr = round( max( 2000000 / ( Nrd * Nrr ), 10 ) );   % allow space for at least 10 arrivals
   fprintf( '\n( Maximum # of arrivals = %i ) \n', MxNArr );
   
   AArr   = zeros( Nrd, Nrr, MxNArr );
   DelArr = zeros( Nrd, Nrr, MxNArr );
   AngArr = zeros( Nrd, Nrr, MxNArr );
   NArr   = zeros( Nrd, Nrr );
end

omega = 2 * pi * freq;

if ( Nrr > 1 )
   Deltar = rr( Nrr ) - rr( Nrr - 1 );
else
   Deltar = 0.0;
end

alpha = ( pi / 180 ) * alpha;   % convert to radians
Dalpha = 0.0;
if ( Nbeams ~= 1 )
   Dalpha = ( alpha( Nbeams ) - alpha( 1 ) ) / Nbeams;  % angular spacing between beams
end

% *** Loop over source depths ***

for is = 1:Nsd
   xs = [ 0.0 sd( is ) ];   % source coordinate
   
   [ c, gradc, crr, crz, czz, Layer ] = ssp( xs, zSSPV, cSSPV, czV, TopOpt );
   RadMax = 10 * c / freq;  % 10 wavelength max radius
   
   % Are there enough beams?
   DalphaOpt = sqrt( c / ( 6.0 * freq * rr( Nrr ) ) );
   NbeamsOpt = round( 2 + ( alpha( Nbeams ) - alpha( 1 ) ) / DalphaOpt );
   
   if ( RunType(1:1) == 'C' & Nbeams < NbeamsOpt )
      fprintf( 'Warning: Nbeams should be at least = %i \n', NbeamsOpt )
   end
   
   % *** Trace rays ***
   
   trace( ibeams, deltas, xs, alpha, BeamType, zBox, rBox, BotOpt, RunType )
   
   if RunType(1:1) == 'R'
      figure
      for ibeam = ibeams
         plot( squeeze( xv( ibeam, 1, 1:Nsteps( ibeam ) ) )', ...
            squeeze( xv( ibeam, 2, 1:Nsteps( ibeam ) ) )' );...
            view( 0, -90 ); drawnow; hold on
      end
      %save RAYFIL ibeams Nsteps xv
   else  
      U = zeros( Nrd, Nrr );
      influg( ibeams, sd( is ), alpha, RunType, Dalpha  )
      if ( RunType(1:1) == 'A' )   % arrivals calculation
         save ARRFIL PlotTitle sd Nsd rd Nrd rr Nrr NArr DelArr AArr AngArr MxNArr
         movefile( 'ARRFIL.mat', arrfil );
      else
         p = scalep( Dalpha, c, rr, U, Nrd, Nrr, RunType, TopOpt, freq );
         atten = 0;
         save SHDFIL PlotTitle freq atten sd Nsd rd Nrd rr Nrr p  % problem: sd is outside the loop
         movefile( 'SHDFIL.mat', shdfil );
         plotshd( shdfil )
      end
   end
   
end % next source depth

toc