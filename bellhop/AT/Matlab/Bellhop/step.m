function [ x2, Tray2, p2, q2, tau2, Rfa2, c2 ] = ...
    step(  x0, Tray0, p0, q0, tau0, Rfa0, ...
    xTop, nTop, xBot, nBot, deltas, TopOpt )

global SSP

tau0 = tau0( : ); % make sure everything is a column vector

Nbeams = length( tau0 );

% Does a single step along the ray
% x denotes the ray coordinate, (r,z)
% Tray denotes the scaled tangent to the ray

% *** Phase 1 of modified polygon method (an Euler step) ***

[ c0, gradc0, crr0, crz0, czz0, Layer ] = ssp( x0, SSP );
csq0 = c0 .* c0;
cnn0_csq0 = crr0 .* Tray0(:,2).^2 - 2.0 * crz0 .* Tray0(:,1) .* Tray0(:,2) + czz0 .* Tray0(:,1).^2;

Layer0 = Layer;   % make note of current layer

h = deltas * ones( Nbeams, 1 );   % initially set the step h, to the basic one, deltas
hBot = h;
hTop = h;

x( :, 1 ) = x0( :, 1 ) + h .* c0 .* Tray0( :, 1 );  % make a trial step
x( :, 2 ) = x0( :, 2 ) + h .* c0 .* Tray0( :, 2 );  % make a trial step

% *** Detect interface or boundary crossing

d = x - xTop;              % vector from top    to ray endpoint
CrossTop = uint16( find ( dot( d', nTop' ) > 0.0 ) );

d = x - xBot;              % vector from bottom to ray endpoint
CrossBot = uint16( find ( dot( d', nBot' ) > 0.0 ) );

% *** Adjust step to land on an interface or boundary

CrossIA = uint16( find( x( :, 2) < SSP.z( Layer( : ) ) ) );
if ( isempty( CrossIA ) == 0 )
  h( CrossIA ) = ( SSP.z( Layer(  CrossIA ) ) - x0( CrossIA, 2) ) ./ ...
                 ( Tray0( CrossIA, 2) .* c0( CrossIA ) );
end

Layer1 = double( Layer ); % double variable needed to do arithmetic in next line
Layer1 = Layer1 + 1;
CrossIB = uint16( find( x( :, 2) > SSP.z( Layer1 ) ) );
if ( isempty( CrossIB ) == 0 )
  h( CrossIB ) = ( SSP.z( Layer1( CrossIB ) ) - x0( CrossIB, 2) ) ./ ...
                 ( Tray0( CrossIB, 2) .* c0( CrossIB ) );
end

if ( isempty( CrossTop ) == 0 )
  e( CrossTop, : ) =  x0( CrossTop, : ) - xTop( CrossTop, : );   % vector from top    to ray origin
  hTop( CrossTop ) =       -dot(     e( CrossTop, : )', nTop( CrossTop, : )' ) ...
    ./ ( c0( CrossTop )' .* dot( Tray0( CrossTop, : )', nTop( CrossTop, : )' ) );
  h = min( h, hTop ); % step is limited by the smaller of an interface or top crossing
end

if ( isempty( CrossBot ) == 0 )
e( CrossBot, : ) =  x0( CrossBot, : ) - xBot( CrossBot, : );   % vector from bottom to ray origin
hBot( CrossBot ) =        -dot(     e( CrossBot, : )', nBot( CrossBot, : )' ) ./ ...
    ( c0( CrossBot )' .* ( dot( Tray0( CrossBot, : )', nBot( CrossBot, : )' ) ) );
  h = min( h, hBot );   % step is limited by the smaller of an interface or bottom crossing
end

h = max( h, 1e4 * eps( deltas ) );   % make sure we make some motion
halfh = 0.5 * h;   % first step of the modified polygon method is a half step

x1(    :, 1 ) = x0(    :, 1 ) + halfh .* c0 .* Tray0( :, 1 );
x1(    :, 2 ) = x0(    :, 2 ) + halfh .* c0 .* Tray0( :, 2 );
Tray1( :, 1 ) = Tray0( :, 1 ) - halfh .* gradc0( :, 1 ) ./ csq0;
Tray1( :, 2 ) = Tray0( :, 2 ) - halfh .* gradc0( :, 2 ) ./ csq0;
p1(    :, 1 ) = p0(    :, 1 ) - halfh .* cnn0_csq0 .* q0( :, 1 );
p1(    :, 2 ) = p0(    :, 2 ) - halfh .* cnn0_csq0 .* q0( :, 2 );
q1(    :, 1 ) = q0(    :, 1 ) + halfh .* c0        .* p0( :, 1 );
q1(    :, 2 ) = q0(    :, 2 ) + halfh .* c0        .* p0( :, 2 );

% *** Phase 2 of modified polygon method ***

[ c1, gradc1, crr1, crz1, czz1, Layer ] = ssp( x1, SSP );
csq1 = c1 .* c1;
cnn1_csq1 = crr1 .* Tray1(:,2).^2 - 2.0 * crz1 .* Tray1(:,1) .* Tray1(:,2) + czz1 .* Tray1(:,1).^2;

if ( isempty( CrossIA ) == 0 )
h( CrossIA ) = ( SSP.z( Layer0( CrossIA ) ) - x0( CrossIA, 2) ) ./ ...
               ( Tray1( CrossIA, 2) .* c1( CrossIA ) );
end

if ( isempty( CrossIB ) == 0 )
  h( CrossIB ) = ( SSP.z( Layer1( CrossIB ) ) - x0( CrossIB, 2) ) ./ ...
                 ( Tray1( CrossIB, 2) .* c1( CrossIB ) );
end

if ( isempty( CrossTop ) == 0 )
  hTop( CrossTop ) =      -dot( e(     CrossTop, : )', nTop( CrossTop, : )' ) ./ ...
      ( c1( CrossTop )' .* dot( Tray1( CrossTop, : )', nTop( CrossTop, : )' ) );
  h = min( h, hTop );
end

if ( isempty( CrossBot ) == 0 )
  hBot( CrossBot ) =        -dot(     e( CrossBot, : )', nBot( CrossBot, : )' ) ./ ...
      ( c1( CrossBot )' .* ( dot( Tray1( CrossBot, : )', nBot( CrossBot, : )' ) ) );
  h = min( h, hBot );
end

h = max( h, 1e4 * eps( deltas ) );   % make sure we make some motion

x2(    :, 1 ) = x0(    :, 1 )    + h .* c1 .* Tray1( :, 1 );
x2(    :, 2 ) = x0(    :, 2 )    + h .* c1 .* Tray1( :, 2 );
Tray2( :, 1 ) = Tray0( :, 1 )    - h .* gradc1( :, 1 ) ./ csq1;
Tray2( :, 2 ) = Tray0( :, 2 )    - h .* gradc1( :, 2 ) ./ csq1;
p2(    :, 1 ) = p0(    :, 1 )    - h .* cnn1_csq1 .* q1( :, 1 );
p2(    :, 2 ) = p0(    :, 2 )    - h .* cnn1_csq1 .* q1( :, 2 );
q2(    :, 1 ) = q0(    :, 1 )    + h .* c1        .* p1( :, 1 );
q2(    :, 2 ) = q0(    :, 2 )    + h .* c1        .* p1( :, 2 );
tau2          = tau0             + h ./ c1;
Rfa2          = Rfa0;

% If we crossed an interface, apply jump condition

[ c2, gradc2, crr2, crz2, czz2, Layer ] = ssp( x2, SSP );

ii = find( Layer ~= Layer0 );
if ( isempty( ii ) ==0 )
    RN  = -Tray2( ii, 1).^ 2 ./ Tray2( ii, 2) .* ( gradc2( ii, 2) - gradc0( ii, 2) ) ./ c0( ii );
    % above needs updating for c(r,z) problem
    p2( ii, 1 ) = p2( ii, 1 ) + q2( ii, 1 ) .* RN;
    p2( ii, 2 ) = p2( ii, 2 ) + q2( ii, 2 ) .* RN;
end
