function Density = GetBottomDensity(Env)
%This function obnly makes sense for a 2-layer environment.  Returns an empty matrix for any other environment.

Density = [];

if length(Env.LayerArr) == 2 
   Density = GetMaxDensity(Env.LayerArr{end});
end
