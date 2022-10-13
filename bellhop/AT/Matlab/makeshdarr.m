function makeshdarr( ARRFIL, freqlist, filetype )

% makeshdarr( ARRFIL, freqlist, filetype );
% where ARRFIL is the arrivals files
% freqlist is a vector of frequencies
% filetype indicates ascii or binary format
%
% Creates a pressure field, like the
% contents of a Bellhop .SHD file, using the data in a Bellhop
% produced .ARR arrivals file.
% Chris Tiemann Feb. 2001
% mbp: small mods August 2002

narrmx = 50;
outfilename = ARRFIL; %###.mat
ARRFIL = [ ARRFIL '.arr' ];
%filetype = 'mat'

switch ( filetype(1:3) )
   case ( 'asc' )
      [ AArr, DelArr, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, NArr, Pos ] ...
         = read_arrivals_asc( ARRFIL, narrmx );
   case ( 'mat' )
      load( outfilename )  % confusing stuff here because same *.mat file used for input and output ...
      % Matlab version of Bellhop has a different ordering
      AArr   = permute( AArr,   [ 2 3 1 ] );
      DelArr = permute( DelArr, [ 2 3 1 ] );
      NArr   = NArr';
   case ( 'bin' )
      [ AArr, DelArr, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, NArr, Pos ] ...
         = read_arrivals_bin( ARRFIL, narrmx );
   otherwise
      error( 'Unknown file type' )
end    

Nsd = length( Pos.s.depth );    % # source depths
Nrd = length( Pos.r.depth );    % # receiver depths
Nrr = length( Pos.r.range );    % # receiver ranges

%Fields for different frequencies saved in separate files

%Calculate pressure field for different frequencies
for ifreq = 1:length( freqlist )
   disp( [ 'Calculating pressure field for frequency ' num2str( freqlist( ifreq ) ) ...
         ' Hz.' ] )
   freq = freqlist( ifreq );
   w = 2 * pi * freq;
   p = zeros( Nrd, Nrr, Nsd );
   
   if Nsd > 1
      for isd = 1:Nsd
         for ird = 1:Nrd
            for ir = 1:Nrr
               numarr = NArr( ir, ird, isd );
               
               if numarr>0;
                  p( ird, ir, isd ) = AArr( ir, 1:numarr, ird, isd ) * ...
                       exp( i * w * DelArr( ir, 1:numarr, ird, isd ) )';
               end %if
            end %ir
         end %ird
      end %isd
   else  % handles case of just one source depth, if arrays are not dimensioned for that
      for ird = 1:Nrd
         for ir = 1:Nrr
            numarr = NArr( ir, ird );
            
            if numarr>0;
               p( ird, ir, 1 ) = AArr( ir, 1:numarr, ird ) * ...
                  exp( i * w * DelArr( ir, 1:numarr, ird ) )';
            end %if
         end %ir
      end %ird
      
      %Save results to file, viewable with plotshd.m
      PlotTitle = ' ';
      atten = 0;
      %outfile = [ outfilename num2str( freqlist( ifreq ) ) '.mat' ];
      eval( [ 'save ' outfilename ' PlotTitle freq atten Pos p' ] );
   end
end %freq
