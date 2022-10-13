function Num = GetNumLayers(Obj)
% returns number of layers in environment ... if object is not a LayerArr
% then -1 is returned

if isempty(Obj.LayerArr)
  	Num = -1;
else
  	Num = length(Obj.LayerArr);
end
