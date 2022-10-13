function Zw = GetWaterDepth(EnvObj)
%Find water depth.
%This is determined as the depth of the top of the second shallowest layer (the shallowest is the water column)
if isempty(EnvObj.LayerArr)
   Zw = [];
else
   Zw = GetLayerThickness(EnvObj.LayerArr{1});
end

   