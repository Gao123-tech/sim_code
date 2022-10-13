function WaterLayer = GetWaterColumn(EnvObj)
%Returns the layer corresponding to the water column.
%This is the first layer in the array
if isempty(EnvObj.LayerArr)
   WaterLayer = [];
else
   WaterLayer = EnvObj.LayerArr{1};
end
