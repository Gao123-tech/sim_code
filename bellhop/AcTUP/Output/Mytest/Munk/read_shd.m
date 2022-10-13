function [ PlotTitle, freq, atten, Pos, p ] = read_shd( filename )

% calls the appropriate routine (binary, ascii, or mat file) to read in the pressure field
% usage: [ pltitl, freq, atten, nsd, nrd, nrr, sd, rd, rr, p ] = read_shd( filename );
% Recommended to include a file extension, if it exists.
% Otherwise it may find a different file than you intended.
% mbp

% if omitted, take a guess at the extension
% Matlab 'exist' command is simpler; however, it searches the whole Matlab
% search path

%determine type of file:

switch filename
   case 'SHDFIL'
      FileType = 'shd';
   case 'ASCFIL'
      FileType = 'asc';
   case 'GRNFIL'
      FileType = 'grn';
   case 'GRNFIL.mat'
      FileType = 'grnmat';
   otherwise
      endchar = length( filename );
      if ( endchar >= 4 )
         FileType = lower( filename( endchar-2 : endchar ) );
      end
end

switch FileType
   case 'shd' % binary format
      [ PlotTitle, freq, atten, Pos, p ] = read_shd_bin( filename );
   case 'mat' % Matlab format
      load( filename );
   case 'asc' % ascii format
      [ PlotTitle, freq, atten, Pos, p ] = read_shd_asc( filename );
   case 'grn' % binary format (Green's function file)
      [ PlotTitle, freq, atten, Pos, p ] = read_shd_bin( filename );
   case 'grnmat'   % Green's function mat file
      load GRNFIL
   otherwise
      warndlg( 'unrecognized file extension', 'Warning' )
end

% clean up PlotTitle by only taking the part up to the closing '
K = findstr( PlotTitle, '''' );   % find indices of ' in PlotTitle
if length( K ) >=2
    PlotTitle = PlotTitle( 1 : K( 2 ) );  % take from the beginning to the second quote
end