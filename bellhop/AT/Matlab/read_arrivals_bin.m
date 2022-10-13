function [ amp, delay, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, narrmat, Pos ] ...
    = read_arrivals_bin( ARRFIL, narrmx )

% read_arrivals_bin
% useage:
%[ amp, delay, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, narrmat, Pos ] ...
%    = read_arrivals_bin( ARRFIL, narrmx );
%
% Loads the arrival time/amplitude data computed by BELLHOP
% You must set the string ARRFIL specifying the Arrivals File
% narrmx specifies the maximum number of arrivals allowed
% mbp 9/96

if nargin == 1
    narrmx = 50;
end

fid = fopen( ARRFIL, 'rb');	% open the file

% read the header info

fseek(fid,4,-1);
freq = fread( fid, 1, 'float' );
Nsd  = fread( fid, 1, 'long'  );
Nrd  = fread( fid, 1, 'long'  );
Nrr  = fread( fid, 1, 'long'  );
fseek(fid,8,0);
Pos.s.depth   = fread( fid, Nsd,'float' );
fseek(fid,8,0);
Pos.r.depth   = fread( fid, Nrd,'float' );
fseek(fid,8,0);
Pos.r.range   = fread( fid, Nrr, 'float' );
fseek(fid,8,0);

% loop to read all the arrival info (delay and amplitude)

amp       = zeros( Nrr, narrmx, Nrd, Nsd );
delay     = zeros( Nrr, narrmx, Nrd, Nsd );
SrcAngle  = zeros( Nrr, narrmx, Nrd, Nsd );
RcvrAngle = zeros( Nrr, narrmx, Nrd, Nsd );
NumTopBnc = zeros( Nrr, narrmx, Nrd, Nsd );
NumBotBnc = zeros( Nrr, narrmx, Nrd, Nsd );

for isd = 1:Nsd
  narrmx2 = fread( fid, 1, 'short', 8 );
  for ird = 1:Nrd
     for ir = 1:Nrr
      
      narr = fread( fid, 1, 'short', 8 );
      narrmat( ir, ird, isd ) = narr;

      if narr > 0   % do we have any arrivals?
        da = fread( fid, [9, narr], 'float');
        amp(       ir, 1:narr, ird, isd ) = da( 1, 1:narr ) .* exp( i * da( 2, 1:narr ) * pi/180 );
        delay(     ir, 1:narr, ird, isd ) = da( 3, 1:narr );
        %SrcAngle(  ir, 1:narr, ird, isd ) = da( 4, : );
        %RcvrAngle( ir, 1:narr, ird, isd ) = da( 5, : );
        %NumTopBnc( ir, 1:narr, ird, isd ) = da( 6, : );
        %NumBotBnc( ir, 1:narr, ird, isd ) = da( 7, : );
      end
    end		% next receiver range
  end		% next receiver depth
end	% next source depth

fclose( fid );
