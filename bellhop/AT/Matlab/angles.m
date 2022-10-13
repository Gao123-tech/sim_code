
function angle( c1, c2 )
% angles
% useage: angle( c1, c2 )
% Prompts for two sound speed values and uses Snell's law to calculate
% the launch angle which which would produce a horizontal ray at the depth of the second sound speed
% mbp 26 Feb. 2001

angle = 180.0 / pi * acos( c1 / c2 )