function [freq, nlayers, dep, zz_out, cc_out, alpha_out, rho_out]=readenv( fname 
)
% READENV - read the environment file for normal mode code
% [freq, nlayers, dep, zz, cc, alpha, rho]=readenv( fname ) 
%   fname: file to read the environment from
%  
%   freq: frequency of source  
%   nlayers: number of layers in model
%   dep:     array of interface depths
%   zz:  depth of mesh points
%   cc:  sound speed profile at zz_out
%   alpha:  attenuation at zz_out
%   rho:  density at zz_out
%  
% Mike Hamilton
% MPL/SIO/UCSD   date: 2000-01-24

fid = fopen( fname );

%---Read file line by line and parse (# is comment char)
line    = fgetl( fid );
line    = fgetl( fid );
freq    = str2num(strtok( line));

line    = fgetl( fid );
nlayers = str2num(strtok( line ));
line  = fgetl( fid );

%---Define the depth array---
dep = zeros(1,nlayers);
zz  = 0;

%---Loop over each layer and read in---
for ii=1:1:nlayers
  %--- Get the npts and depth: first line ---
  line  = fgetl( fid );
  [npts,line] = strtok( line )
  npts = str2num( npts )
  
  [ rough, line ] = strtok( line )
  rough = str2num( rough )
  
  token = strtok( line )
  dep(ii) = str2num( token )

  %---Initialize the arrays---
  line          = fgetl( fid );
  [token, line] = strtok( line );
  zz            = str2num( token );
  [token, line] = strtok( line );
  cc            = str2num( token );
  [token, line] = strtok( line );
  %alpha         = str2num( token );
  %[token, line] = strtok( line );
  %rho           = str2num( token );
  kk = 1;
  %---Read in the rest of the data---
  while( zz(kk) ~= dep(ii) )
    line          = fgetl( fid );
    [token, line] = strtok( line );
    zz            = [zz, str2num( token ) ];
    [token, line] = strtok( line );
    cc            = [cc, str2num( token )];
    [token, line] = strtok( line );
    %alpha         = [alpha, str2num( token )];
    %[token, line] = strtok( line );
    %rho           = [rho, str2num( token )];
    kk = kk + 1;
  end

  %---Interpolate to the proper num of mesh points---
  %---and add to array-------------------------------
  zz_new    = linspace(zz(1),dep(ii),npts);
  cc_new    = interp1(zz,   cc,zz_new);
  alpha_new = interp1(zz,alpha,zz_new);
  rho_new   = interp1(zz,  rho,zz_new);
  if( exist( 'zz_out' ) ) 
      zz_out    = [zz_out, zz_new];
      cc_out    = [cc_out, cc_new];
      alpha_out = [alpha_out, alpha_new];
      rho_out   = [rho_out, rho_new];
  else
      zz_out    = zz_new;
      cc_out    = cc_new;
      alpha_out = alpha_new;
      rho_out   = rho_new;
  end    
end
  
%---Get the Bottom halfspace---
line          = fgetl( fid );
[token, line] = strtok( line );
zz            = str2num( token );
[token, line] = strtok( line );
cc            = str2num( token );
[token, line] = strtok( line );
alpha         = str2num( token );
[token, line] = strtok( line );
rho           = str2num( token );

zz_out    = [zz_out, zz];
cc_out    = [cc_out, cc];
alpha_out = [alpha_out, alpha];
rho_out   = [rho_out, rho];
 
fclose( fid );
