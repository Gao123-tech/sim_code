function [ title, freq, atten, Pos, tlt ] = read_shd_bin( filename )

% Reads in TL surfaces from a binary Bellhop/Kraken .SHD file 
% without having to convert to ASCII first.
% Chris Tiemann, Feb. 2001

fid = fopen( filename, 'rb' );
if ( fid == -1 )
    error( 'No shade file with that name exists; you must run a model first' );
end

recl  = fread( fid,  1, 'int32' );     %record length in bytes will be 4*recl
title = fread( fid, 80, '*char' )';

fseek(fid, 4*recl, -1); %reposition to end of first record
plottype = fread( fid, 10, 'uchar' );
xs    = fread( fid, 1, 'float32' );
ys    = fread( fid, 1, 'float32' );
theta = fread( fid, 1, 'float32' );

fseek(fid, 2 * 4 * recl, -1 ); %reposition to end of second record
freq   = fread( fid, 1, 'float32' ); 
Nsd    = fread( fid, 1, 'int32'   );
Nrd    = fread( fid, 1, 'int32'   );
Nrr    = fread( fid, 1, 'int32'   );
atten  = fread( fid, 1, 'float32' ); 

fseek( fid, 3 * 4 * recl, -1 ); %reposition to end of third record
Pos.s.depth = fread( fid, Nsd, 'float32' );

fseek( fid, 4 * 4 * recl, -1 ); %reposition to end of fourth record
Pos.r.depth = fread( fid, Nrd, 'float32' );

fseek( fid, 5 * 4 * recl, -1 ); %reposition to end of fifth record
Pos.r.range = fread( fid, Nrr, 'float32' );

%Each record holds data from one source depth/receiver depth pair

tlt = zeros( Nsd, Nrd, Nrr );

for isd = 1:Nsd
  disp( [ 'Reading data for source ' num2str( isd ) ' of ' num2str( Nsd ) ] )
  for ird = 1:Nrd
    recnum = 6 + ( isd - 1 ) * Nrd + ird - 1;
    status = fseek( fid, recnum * 4 * recl, -1 ); %Move to end of previous record
    if ( status == -1 )
        error( 'Seek to specified record failed in read_shd_bin' )
    end
 
    temp = fread( fid, 2 * Nrr, 'float32' );    %Read complex data
    tlt( isd, ird, : ) = temp( 1 : 2 : 2 * Nrr ) + i * temp( 2 : 2 : 2 * Nrr ); 
    %Transmission loss matrix indexed by  sd x rd x rr

  end
end

fclose( fid );