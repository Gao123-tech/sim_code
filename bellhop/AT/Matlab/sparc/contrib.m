function [ AD0, AE0, AD1, AE1, AD2, AE2 ] = contrib( Node, x, rkT, V, rhoElt, c2RElt, c2IElt, hElt, deltat, alpha, beta, AD0, AE0, AD1, AE1, AD2, AE2 )

% Computes the contribution for the given element

%  alpha controls lumping and beta controls implicitness

deltat2 = deltat^2;
rhoh    = rhoElt * hElt;

%Ke = zeros( 2, 2 );
%Ce = zeros( 2, 2 );
%Me = zeros( 2, 2 );

% * Elemental stiffness matrix ( +k2 term ) *

y = 1.0 + i * rkT * V * c2IElt;
z = hElt * x * ( y - V * V / c2RElt ) / rhoElt / 6.0;
Ke( 1, 1 ) =  y / rhoh + (3.0-alpha) * z;
Ke( 1, 2 ) = -y / rhoh +      alpha  * z;
Ke( 2, 2 ) =  y / rhoh + (3.0-alpha) * z;

% * Elemental damping matrix *

xx = hElt * x / rhoElt / 6.0;
z  = hElt * 2.0 * i * rkT * V / c2RElt / rhoElt / 6.0;
Ce( 1, 1 ) = c2IElt * ( 1.0/rhoh + (3.0-alpha) * xx) + (3.0-alpha) * z;
Ce( 1, 2 ) = c2IElt * (-1.0/rhoh +      alpha  * xx) +      alpha  * z;
Ce( 2, 2 ) = c2IElt * ( 1.0/rhoh + (3.0-alpha) * xx) + (3.0-alpha) * z;

% * Elemental mass matrix *

Me( 1, 1 ) = (3.0-alpha) * hElt / ( rhoElt * c2RElt ) / 6.0;
Me( 1, 2 ) =      alpha  * hElt / ( rhoElt * c2RElt ) / 6.0;
Me( 2, 2 ) = (3.0-alpha) * hElt / ( rhoElt * c2RElt ) / 6.0;

% * A2 matrix *

AD2( Node   ) = AD2( Node ) + ...
                Me(1,1) + 0.5 * deltat * Ce(1,1) + beta * deltat2 * Ke(1,1);
AE2( Node+1 ) = Me(1,2) + 0.5 * deltat * Ce(1,2) + beta * deltat2 * Ke(1,2);
AD2( Node+1 ) = Me(2,2) + 0.5 * deltat * Ce(2,2) + beta * deltat2 * Ke(2,2);

% * A1 matrix *

AD1( Node   ) = AD1( Node ) + ...
                2.0*Me(1,1) - ( 1.0 - 2.0*beta ) * deltat2 * Ke(1,1);
AE1( Node+1 ) = 2.0*Me(1,2) - ( 1.0 - 2.0*beta ) * deltat2 * Ke(1,2);
AD1( Node+1 ) = 2.0*Me(2,2) - ( 1.0 - 2.0*beta ) * deltat2 * Ke(2,2);

% * A0 matrix *

AD0( Node   ) = AD0( Node ) ...
                -Me(1,1) + 0.5 * deltat * Ce(1,1) - beta * deltat2 * Ke(1,1);
AE0( Node+1 ) = -Me(1,2) + 0.5 * deltat * Ce(1,2) - beta * deltat2 * Ke(1,2);
AD0( Node+1 ) = -Me(2,2) + 0.5 * deltat * Ce(2,2) - beta * deltat2 * Ke(2,2);
