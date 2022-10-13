function [ pltitl, freq, ck, z, phi, cpt, cst, rhot, deptht, cpb, csb, rhob, depthb, Nmedia, depth, rho ] = read_modes_asc( filename, modes )

% read_modes
% useage:
%    [ pltitl, freq, ck, z, phi ] = read_modes_asc( filename, modes )
% read the modes produced by KRAKEN
% filename should include extension
% modes is an optional vector of mode indices

fid = fopen( filename, 'r' );

lrecl   = fscanf( fid, '%i' );
pltitl = fgetl( fid );

temp = fscanf( fid, '%f', [ 5 ] );
freq   = temp( 1 );
nmedia = temp( 2 );
ntot   = temp( 3 );
nmat   = temp( 4 );
m      = temp( 5 );

%temp   = fscanf( fid, '%i', [ nmedia ] )
for ii = 1 : nmedia
  junk   = fgetl( fid );
end
junk   = fgetl( fid );   % top halfspace properties
junk   = fgetl( fid );   % bot halfspace properties
junk   = fgetl( fid );   % blank line

z      = fscanf( fid, '%f', [ 1, ntot ] );
junk   = fgetl( fid );
ckt    = fscanf( fid, '%f', [ 2, m ] );
ck = ckt( 1, : )' + i * ckt( 2, : )';

if nargin == 1
    modes = 1:m;    % read all modes if the user didn't specify
end

% don't try to read modes that don't exist
ii = find( modes <= m );
modes = modes( ii );
   
phi = zeros( ntot, length( modes ) );

for mode = 1: max( modes )
   junk = fgetl( fid );
   phit = fscanf( fid, '%f', [ 2, ntot] );  % read the mode
   
   ii = 0;
   ii = find( mode == modes );  % see if it's in the list to grab
   if ii >= 1                   % if yes, the store it
      phi( :, ii )  = phit( 1, : )' + i * phit( 2, : )';
  end
end

%figure; imagesc( 1:m, z, real( phi ) ); colorbar
%figure; surf( 1:m, z, real( phi ) ); colorbar

