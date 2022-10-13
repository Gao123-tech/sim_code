function bellhop( filename )

% Beam-tracing code for ocean acoustics problems
% Based on the earlier Fortran version
% MBP Dec. 30, 2001
%
% For Matlab, the code had to be vectorized so all beams are traced in parallel.
% Furthermore, to keep the various vectors associated with each ray
% together in memory, it was necessary to collect them in a structure.
%
% The tracing of beams in parallel lead to huge storage requirements for the ray trajectories
% Suggests a possible rewrite to calculate TL (INFLUG module) in parallel
% with the ray trace (TRACE module).
% In the meantime, note that the run time goes through the ceiling when the
% required storage gets too large ...

clear global
tic
global SSP

if ( isempty( filename ) )
   warndlg( 'No envfil has been selected', 'Warning' );
end

global ray Nsteps omega Bdry
global Pos
global alpha Nbeams
global U NArr DelArr AArr AngArr MxNArr

% filenames
envfil = [ filename '.env' ];   % input environmental file
shdfil = [ filename '.mat' ];   % output file name (pressure)
arrfil = [ filename '.mat' ];   % output file name (arrivals)
btyfil = [ filename '.bty' ];   % input bathymetry
atifil = [ filename '.ati' ];   % input altimetry
sbpfil = [ filename '.sbp' ];   % input source beam pattern
trcfil = [ filename '.trc' ];   % input top    reflection coefficient
brcfil = [ filename '.brc' ];   % input bottom reflection coefficient

[ PlotTitle, freq, Bdry, fid ] = readenv( envfil );    % read in the environmental file
PlotTitle = ['BELLHOP(M) -' PlotTitle ];

Pos = readsdrd( fid );                           % read in the source and receiver depths
[ Pos.r.range, Pos.Nrr ] = readr(    fid );     % read in receiver ranges

[ Isingl, Nimage, Ibwin, deltas, zBox, rBox, epmult, rLoop, RunType, BeamType ] = read_bell( fid, freq, Bdry.Bot.depth, Bdry.Top.depth, Pos.r.range( Pos.Nrr) );

ibeams = 1 : Nbeams;   % select beams to trace
Nbeams = length( ibeams );

TopATI = Bdry.Top.Opt(4:4);
readati( atifil, TopATI, Bdry.Top.depth, rBox );    % read in the altimetry  file

BotBTY = Bdry.Bot.Opt(2:2);
readbty( btyfil, BotBTY, Bdry.Bot.depth, rBox );    % read in the bathymetry file

readrc( trcfil, brcfil, Bdry.Top.Opt(2:2), Bdry.Bot.Opt(1:1) );   % READ Reflection Coefficients (top and bottom)
SrcBmPat = readpat( sbpfil, RunType(3:3) );   % optionally read source beam pattern

if ( ismember( 'A', upper( RunType(1:1) ) ) )
   MxNArr = round( max( 2000000 / ( Pos.Nrd * Pos.Nrr ), 10 ) );   % allow space for at least 10 arrivals
   fprintf( '\n( Maximum # of arrivals = %i ) \n', MxNArr );
   
   AArr   = zeros( Pos.Nrd, Pos.Nrr, MxNArr );
   DelArr = zeros( Pos.Nrd, Pos.Nrr, MxNArr );
   AngArr = zeros( Pos.Nrd, Pos.Nrr, MxNArr );
   NArr   = zeros( Pos.Nrd, Pos.Nrr );
end

omega = 2 * pi * freq;

if ( Pos.Nrr > 1 )
   Deltar = Pos.r.range( Pos.Nrr ) - Pos.r.range( Pos.Nrr - 1 );
else
   Deltar = 0.0;
end

Amp0 = interp1( SrcBmPat( :, 1 ), SrcBmPat( :, 2 ), alpha ); % calculate initial beam amplitude based on source beam pattern

alpha = ( pi / 180 ) * alpha;   % convert to radians
Dalpha = 0.0;
if ( Nbeams ~= 1 )
   Dalpha = ( alpha( Nbeams ) - alpha( 1 ) ) / ( Nbeams - 1 );  % angular spacing between beams
end

% *** Loop over source depths ***

for is = 1:Pos.Nsd
   xs = [ 0.0 Pos.s.depth( is ) ];   % source coordinate
   [ c, gradc, crr, crz, czz, Layer ] = ssp( xs, SSP );
   RadMax = 10 * c / freq;  % 10 wavelength max radius
   
   % Are there enough beams?
   DalphaOpt = sqrt( c / ( 6.0 * freq * Pos.r.range( Pos.Nrr ) ) );
   NbeamsOpt = round( 2 + ( alpha( Nbeams ) - alpha( 1 ) ) / DalphaOpt );
   
   if ( RunType(1:1) == 'C' && Nbeams < NbeamsOpt )
      fprintf( 'Warning: Nbeams should be at least = %i \n', NbeamsOpt )
   end
   
   % *** Trace rays ***
   
   trace( ibeams, deltas, xs, alpha, Amp0, BeamType, zBox, rBox, Bdry.Bot.Opt, RunType )
   
   if RunType(1:1) == 'R'
      figure
      title( PlotTitle );
      set( gca, 'YDir', 'Reverse' )   % plot with depth-axis positive down

      for ibeam = ibeams
         r = zeros( Nsteps( ibeam ), 1 );
         z = zeros( Nsteps( ibeam ), 1 );
         for ii = 1:Nsteps( ibeam )
            r( ii ) = ray( ii ).x( ibeam, 1 );
            z( ii ) = ray( ii ).x( ibeam, 2 );
         end
         plot( r, z ); drawnow; hold on
         set( gca, 'YDir', 'Reverse' )   % plot with depth-axis positive down
      end
      ii = max( Nsteps );
      save RAYFIL ibeams Nsteps ray
   else  
      U = zeros( Pos.Nrd, Pos.Nrr );
      if ( RunType(2:2) == 'G' )
         influg(   ibeams, Pos.s.depth( is ), alpha, RunType, Dalpha  )
      else
         influgrb( ibeams, Pos.s.depth( is ), alpha, RunType, Dalpha  )
      end
      
      if ( RunType(1:1) == 'A' )   % arrivals calculation
         
         % *** Thorpe attenuation? ***
         if ( Bdry.Top.Opt(4:4) == 'T' )
            f2 = ( freq / 1000.0 )^2;
            alpha = 40.0 * f2 / ( 4100.0 + f2 ) + 0.1 * f2 / ( 1.0 + f2 );
            alpha = alpha / 914.4;     % dB / m
            alpha = alpha / 8.6858896; % Nepers / m
         else
            alpha = 0.0;
         end
         
         % add in attenuation
         for ir = 1 : Pos.Nrr
            factor = exp( -alpha * Pos.r.range( ir ) ) / sqrt( Pos.r.range( ir ) );
            AArr( :, ir, : ) = factor * AArr( :, ir, : );
         end
         save ARRFIL PlotTitle Pos NArr DelArr AArr AngArr MxNArr
         movefile( 'ARRFIL.mat', arrfil );
      else
         p = scalep( Dalpha, c, Pos.r.range, U, Pos.Nrr, RunType, Bdry.Top.Opt, freq );
         atten = 0;
         save SHDFIL PlotTitle freq atten Pos p  % problem: sd is outside the loop
         movefile( 'SHDFIL.mat', shdfil );
         %plotshd( shdfil )
      end
   end
   
end % next source depth

toc